use strict; use warnings;
no warnings 'experimental::signatures';
use feature 'signatures';

package YS::YSReader;

use YS::Types;
use YS::Reader;

use Scalar::Util 'refaddr';

our %events;
our %functions;
our %refs;

#------------------------------------------------------------------------------
# Convert YAMLScript into a Mal Lisp AST
#------------------------------------------------------------------------------
sub read_file {
    my ($yaml, $file) = @_;

    %events = ();
    %functions = ();
    %refs = ();

    my $self = bless {
        from => "$file",
        text => "$yaml",
    }, __PACKAGE__;

    $self->{events} = $self->parse_yaml_pp($yaml);
    my $dom = $self->compose_dom;
    my $ast = $self->construct_ast($dom);

    return $ast;
}

our @event_keys = (qw<
    type
    bpos blin bcol
    epos elin ecol
    anch ytag
    styl valu
>);

sub parse_yaml_fy {
    my ($self, $yaml) = @_;

    require IPC::Run;

    my ($out, $err);
    IPC::Run::run(
        [qw< fy-tool --testsuite --tsv-format >],
        $yaml,
        \$out,
        \$err,
        IPC::Run::timeout(5),
    );

    [ map 'event'->new($_), split /\n/, $out ];
}

my $event_dict = {
    stream_start_event   => '+str',
    stream_end_event     => '-str',
    document_start_event => '+doc',
    document_end_event   => '-doc',
    mapping_start_event  => '+map',
    mapping_end_event    => '-map',
    sequence_start_event => '+seq',
    sequence_end_event   => '-seq',
    scalar_event         => '=val',
    alias_event          => '=ali',
};

sub parse_yaml_pp {
    my ($self, $yaml) = @_;
    require YAML::PP::Parser;
    my $events = [];
    YAML::PP::Parser->new(
        receiver => sub {
            my ($self, undef, $event) = @_;
            my @event = (
                ($event_dict->{$event->{name}} || XXX($event)),
                0, 0, 0, 0, 0, 0,
                ($event->{anchor} || '-'),
                ($event->{tag} || '-'),
            );
            if ($event->{name} eq 'scalar_event') {
                my $value = $event->{value};
                my $style = $event->{style};
                $value =~ s/\\/\\\\/g;
                $value =~ s/\n/\\n/g;
                push @event,
                    (
                        $style == 1 ? ':' :
                        $style == 4 ? '|' :
                        '"'
                    ),
                    $value;
            }
            push @$events, join "\t", @event;
        },
    )->parse_string($yaml);
    [ map 'event'->new($_), @$events ];
}

#------------------------------------------------------------------------------
# AST Implicit Typing Methods
#------------------------------------------------------------------------------

my $E_GROUP = 'event'->new("=xxx\t-1\t-1\t-1\t-1\t-1\t-1\t-\t-\t-\t-");
my $E_PLAIN = 'event'->new("=xxx\t-1\t-1\t-1\t-1\t-1\t-1\t-\t-\t:\t-");
my $E_QUOTE = 'event'->new("=xxx\t-1\t-1\t-1\t-1\t-1\t-1\t-\t-\t'\t-");
sub PAIR { 'pair'->new(@_) }
sub MAP { 'map'->new($E_GROUP, @_) }
sub SEQ { 'seq'->new($E_GROUP, @_) }
sub VAL { 'val'->new($E_PLAIN, @_) }
sub STR { 'val'->new($E_QUOTE, @_) }

sub B { boolean($_[0]) }
sub K { keyword(@_) }
sub L { list([@_]) }
sub N { number(@_) }
sub S { symbol($_[0]) }
sub T { string(@_) }

sub DEF { S 'def!' }
sub DO { S 'do' }
sub FN { S 'fn*' }
sub IF { S 'if' }
sub LET { S 'let*' }

my $sym = qr<[-\w]+[\?\!\*]?>;

sub error($m) { die "YS Error: $m\n" }
sub event($n) { $events{refaddr($n)} }
sub e_style($n) { event($n)->{styl} }
sub e_tag($n) { event($n)->{ytag} }
sub is_map($n) { ref($n) eq 'map' }
sub is_seq($n) { ref($n) eq 'seq' }
sub is_val($n) { ref($n) eq 'val' }
sub is_pair($n) { ref($n) eq 'pair' }
sub is_key($n) { $n->{xkey} }
sub is_plain($n) { is_val($n) and e_style($n) eq ':' }
sub is_literal($n) { is_val($n) and e_style($n) eq '|' }
sub is_single($n) { is_map($n) and pairs($n) == 1 }
sub is_assign($n) {
  is_single($n) and
  text(key(first_pair($n))) =~ /^$sym\s+=$/;
}
sub is_def($n) { is_map($n) and tag(key(first_pair($n))) eq 'def' }

sub assert_map($n) { is_map($n) or ZZZ($n) }
sub assert_seq($n) { is_seq($n) or ZZZ($n) }
sub assert_val($n) { is_val($n) or ZZZ($n) }
sub assert_pair($n) { is_pair($n) or ZZZ($n) }
sub assert_elems($n) { assert_seq($n); @{$n->elem} > 0 or ZZZ($n) }
sub assert_pairs($n) { assert_map($n); @{$n->pair} > 0 or ZZZ($n) }
sub pairs($n) { assert_map($n); @{$n->pair} }
sub elems($n) { assert_seq($n); @{$n->elem} }
sub tag($n) { $n->{ytag} }
sub key($p) { assert_pair($p); $p->key }
sub val($p) { assert_pair($p); $p->val }
sub key_val($p) { assert_pair($p); @$p }
sub text($v) { assert_val($v); $v->{text} }
sub first_elem($n) { assert_elems($n); (elems($n))[0] }
sub first_pair($n) { assert_pairs($n); (pairs($n))[0] }

sub construct_ast($s, $n) {
    if (is_val($n)) {
        my $text = $n->{text};
        $n->{text} = "(do\n$text\nnil)";

    } elsif (is_seq($n)) {
        my $pair = PAIR( VAL('main()') => SEQ(elems($n)) );
        $pair->[0]{ytag} = 'defn';
        $n = MAP($pair);
        $n->{ytag} = 'module';
    }

    my $ast = $s->construct($n);

    if (has_main($ast)) {
        $ast = L(
            DO,
            $ast,
            L( S('main') ),
        );
    }

    return $ast;
}

sub construct($s, $n) {
    my $tag = is_pair($n) ? tag(key($n)) : tag($n);
    my $constructor;
    if (not $tag) {
        if (is_seq($n)) {
            $constructor = 'construct_do';
        } else {
            XXX $n, "Node has no tag";
        }
    } else {
        $constructor = "construct_$tag";
    }
    $s->$constructor($n);
}

sub construct_boolean($s, $n) {
    "$n" eq 'true' ? true :
    "$n" eq 'false' ? false :
    die;
}

sub construct_callpair($s, $p) {
    my ($key, $value) = @$p;
    "$key" =~ /^($sym):?$/ or die;
    my $fn = $1;
    $fn =~ s/^(let|try|catch)$/$1*/;
    L(S($fn), map $s->construct($_), elems($value));
}

sub construct_def($s, $p) {
    my ($key, $value) = @$p;
    "$key" =~ /^($sym)\s*=$/ or die;
    my $sym = S($1);
    my $rhs = $s->construct($value);
    return L(DEF, $sym, $rhs);
}

sub construct_defn($s, $p) {
    my ($key, $value) = @$p;
    text($key) =~ /^($sym)\((.*)\)$/ or die;
    my $name = S($1);
    my $sig = L(map symbol($_), split /\s+/, $2);
    my $defn = L( DEF, $name, L( FN, L, nil ) );
    my $seq = is_seq($value) ? $value : SEQ($value);
    my @body = is_def(first_elem($seq))
        ? ($s->construct_let($seq))
        : map $s->construct($_), @{$seq->elem};
    return L(DEF, $name, L(FN, $sig, @body));
}

sub construct_do($s, $n) {
    my @elems = elems($n);
    if (@elems == 1) {
        $s->construct($elems[0]);
    } else {
        L(
            DO,
            map $s->construct($_), @elems,
        );
    }
}

sub construct_if($s, $p) {
    my ($key, $value) = @$p;
    "$key" =~ /^if +(.*)/ or die;
    my $cond = read_ysexpr($1);
    L(
        S('if'),
        $cond,
        map $s->construct($_), elems($value),
    );
}

sub construct_int($s, $n) { N("$n") }

sub construct_istr($s, $n) {
    L(
        S('str'),
        map {
            /^\$($sym)$/ ? S($1) :
            /^\$(\(.*\))$/ ? read_ysexpr($1) :
            T($_)
        }
        grep length,
        split /(\$$sym|\$\(.*?\))/, "$n"
    );
}

sub construct_keyword($s, $n) {
    K("$n");
}

sub construct_let($s, $n) {
    my @elems = elems($n);
    my @defs;
    while (@elems and is_def($elems[0])) {
        my $def = $s->construct(shift @elems)->[-1];
        shift @$def;
        push @defs, @$def;
    }
    L(
        S('let*'),
        L(@defs),
        map $s->construct($_), @elems,
    );
}

sub construct_let1($s, $n) {
    my @elems = elems($n->[1]);
    my $assigns = shift @elems or die;
    my $defs = [];
    if (is_map($assigns)) {
        for my $pair (pairs($assigns)) {
            my ($key, $val) = @$pair;
            $key = "$key";
            $key =~ s/\ +=$// or die;
            push @$defs, S($key);
            push @$defs, $s->construct($val);
        }
    } elsif (is_seq($assigns)) {
        XXX $n;
    } else {
        XXX $n;
    }

    L(
        S('let*'),
        $defs,
        map $s->construct($_), @elems,
    );
}

sub construct_ysexpr($s, $n) {
    read_ysexpr($n);
}

sub construct_module($s, $n) {
    L(DO, map $s->construct($_), pairs($n));
}

sub construct_str($s, $n) {
    T("$n");
}

sub construct_sym($s, $n) {
    S("$n");
}

sub construct_try($s, $p) {
    L(
        S('try*'),
        map $s->construct($_),
        map {
            is_map($_) ? first_pair($_) : $_
        } elems(val($p)),
    );
}

sub construct_catch($s, $p) {
    key($p) =~ /^catch\(($sym)\)$/ or die;
    L(
        S('catch*'),
        S($1),
        $s->construct(val($p)),
    );
}

sub is_main($n) {
    ref($n) eq 'list' and
    @$n == 3 and
    ref($n->[0]) eq 'symbol' and
    $n->[0] eq 'def!' and
    ref($n->[1]) eq 'symbol' and
    $n->[1] eq 'main' and
    ref($n->[2]) eq 'list' and
    @{$n->[2]} >= 3 and
    $n->[2][0] eq 'fn*' and
    1;
}

sub has_main($ast) {
    return 1 if is_main($ast);
    return 0 unless ref($ast) eq 'list';
    for my $node (@$ast) {
        return 1 if is_main($node);
    }
    return 0;
}

#------------------------------------------------------------------------------
# YS expression reader.
#
# Converts these special forms:
# x(...)        -> (x ...)
# (x + y)       -> (+ x y)
# (x + y * z)   -> (+ x (* y z))
# x(y + z)      -> (x (+ y z))
#------------------------------------------------------------------------------

my $op = qr{(?:[-+*/]|[<>=]=?|and|or)};
sub tokenize {
    my $ws = qr<(?:]\s,])>;
    my $pn = qr<(?:\('\s|~@|[\[\]\{\}\(\)\~^@])>;
    my $str = qr<"(?:\\.|[^\\"])*"?>;
    my $tok = qr<[^\s\[\]{}('"`,;)]+>;
    [ $_[0] =~ /
        $ws*
        (
            $pn |
            $str |
            $op(?=\s) |
            $sym\( |
            '?$sym |
            '?$tok
        )
    /xog ];
}

sub read_ysexpr($expr) {
    return YS::Reader->new->read_str($expr)
        unless $expr =~ qr<^$sym?\(>;

#     # XXX
#     use Printer;
#     my $expr2 = $expr;
#     $expr2 =~ s/^($sym)\(/($1 /;
#     my $want = Printer::pr_str(YS::Reader->new->read_str($expr2));

    my $tokens = tokenize($expr);
    my $self = bless { tokens => $tokens }, __PACKAGE__;
    my $group = eval {
        my $group = $self->group;
        die "Leftover tokens '@$tokens'"
            if @$tokens;
        $group;
    };
    die "Failed to parser expr '$expr': '$@'" if $@;
    $expr = $self->group_print($group);

    my $ast = YS::Reader->new->read_str($expr);

#     my $got = Printer::pr_str($ast);
#     XXX [$want, $got] unless $got eq $want;
#     return $ast;
}

sub group($s) {
    my $tokens = $s->{tokens};
    my $token = shift @$tokens;
    $token =~ s/^($sym)\($/$1/ ? $s->group_call($token) :
    $token =~ /^\('\s$/ ? $s->group_list(1) :
    $token eq '(' ? $s->group_list(0) :
    die "Unknown token '$token'";
}

sub group_list($s, $l) {
    my $tokens = $s->{tokens};
    my $group = $s->group_rest;
    return $group if $l or @$group != 3 or $group->[1] !~ qr<^$op$>;

    # TODO Support infix group > 3
    [ $group->[1], $group->[0], $group->[2] ];
}

sub group_call($s, $t) {
    my $tokens = $s->{tokens};
    my $group = [$t];
    my $rest = $s->group_rest;
    if (@$rest == 3 and $rest->[1] =~ qr<^$op$>) {
        $rest = [ $rest->[1], $rest->[0], $rest->[2] ];
        $rest = ([$rest]);
    }
    push @$group, @$rest;
    return $group;
}

sub group_rest($s) {
    my $tokens = $s->{tokens};
    my $rest = [];
    while (@$tokens) {
        if ($tokens->[0] eq ')') {
            shift @$tokens;
            return $rest;
        } elsif ($tokens->[0] =~ qr<^$sym?\('?$>) {
            push @$rest, $s->group;
        } else {
            push @$rest, shift @$tokens;
        }
    }
    die "Failed to parse expression";
}

sub group_print($s, $g) {
    my $t = '(';
    for my $e (@$g) {
        $t .= ' ' . (ref($e) ? $s->group_print($e) : $e);
    }
    $t .= ')';
}


#------------------------------------------------------------------------------
# AST Composer Methods
#------------------------------------------------------------------------------
sub compose_dom {
    my ($self) = @_;
    my $node = $self->compose_node('top');
    return $node;
}

sub compose_node {
    my ($self, $flag) = (@_, '');
    my $events = $self->{events};
    while (@$events) {
        my $event = shift(@$events);
        if ($event->{type} =~ /^[+=](map|seq|val|ali)$/) {
            my $composer = "compose_$1";
            my $node = $self->$composer($event);
            $node->{xtop} = 1 if $flag eq 'top';
            $node->{xkey} = 1 if $flag eq 'key';
            $self->tag_node($node);
            return $node;
        }
    }
}

sub compose_map {
    my ($self, $event) = @_;
    my $map = 'map'->new($event);;
    my $events = $self->{events};
    while (@$events) {
        shift(@$events), return $map if $events->[0]{type} eq '-map';
        my $key = $self->compose_node('key');
        my $val = $self->compose_node;
        my $pair = 'pair'->new($key, $val);
        $map->add($pair);
    }
    XXX $map, "problem composing map";
}

sub compose_seq {
    my ($self, $event) = @_;
    my $seq = 'seq'->new($event);
    my $events = $self->{events};
    while (@$events) {
        shift(@$events), return $seq if $events->[0]{type} eq '-seq';
        my $elem = $self->compose_node;
        $seq->add($elem);
    }
    XXX $seq, "problem composing seq";
}

sub compose_val {
    my ($self, $event) = @_;
    'val'->new($event);
}

sub compose_ali {
    my ($self, $event) = @_;
    'ali'->new($event);
}

#------------------------------------------------------------------------------
# AST Tag Resolution Methods
#------------------------------------------------------------------------------

sub tag_node($s, $n) {
    local $_ = $n;
    is_map($n) ? tag_map() :
    is_seq($n) ? tag_seq() :
    is_val($n) ? tag_val() :
    ();
}

sub tag_error($msg) { ZZZ "$msg: '$_'" }

sub tag_map {
    for my $pair (pairs($_)) {
        tag_pair($pair, $_);
    }
    $_->{ytag} = 'module';
}

sub tag_pair {
    my ($pair, $parent) = @_;
    local $_ = key($pair);
    return if $_->{ytag};
    my $text = "$_";
    tag_catch() or
    tag_def() or
    tag_defn() or
    tag_if() or
    tag_let() or
    tag_try() or

    tag_callpair($pair) or
    XXX $pair, "Unable to implicitly tag this map pair.";
}

sub tag_seq {}

sub tag_val {
    if (e_tag($_) ne '-') {
        $_->{ytag} = substr(e_tag($_), 1);
    } elsif (is_literal($_)) {
        tag_istr() or
        tag_str();
    } elsif (is_plain($_)) {
        is_key($_) or
        tag_istr() or
        tag_ysexpr() or
        tag_scalar() or
        tag_sym() or
        tag_error("Unresolvable plain scalar");
    } else {
        tag_str();
    }
}

sub tag_callpair {
    return $_->{ytag} = 'callpair' if /^$sym:$/;
    my ($pair) = @_;
    my $val = val($pair);
    return $_->{ytag} = 'callpair' if /^$sym$/ and is_seq(val($pair));
}

sub tag_catch {
    $_->{ytag} = 'catch' if /^catch\($sym\)$/;
}

sub tag_def {
    $_->{ytag} = 'def' if /^$sym\s*=$/;
}

sub tag_defn {
    $_->{ytag} = 'defn' if /^$sym\((.*)\)$/;
}

sub tag_if {
    $_->{ytag} = 'if' if /^if +\S/;
}

sub tag_istr {
    $_->{ytag} = 'istr' if /(\$$sym|\$\()/;
}

sub tag_let {
    $_->{ytag} = 'let1' if /^let$/;
}

sub tag_scalar {
    $_->{ytag} =
        /^(true|false)$/ ? 'boolean' :
        /^-?\d+$/ ? 'int' :
        /^-?\d+\.\d*$/ ? 'float' :
        /^:$sym$/ ? 'keyword' :
        /^null$/ ? 'null' :
        return;
}

sub tag_str {
    $_->{ytag} = 'str';
}

sub tag_sym {
    $_->{ytag} = 'sym' if /^$sym$/;
}

sub tag_try {
    $_->{ytag} = 'try' if /^try$/;
}

sub tag_ysexpr {
    my $text = $_->{text};
    $text =~ s/^(?: *;.*\n)+//;
    $text =~ s/^[ ,]*//;
    return unless
        $text =~ /^[\\\(]/ or
        $text =~ /^$sym\(.*\)$/;;
    $text =~ s/^\\//;
    $_->{text} = $text;
    $_->{ytag} = 'ysexpr';
}

#------------------------------------------------------------------------------
# Event and Node Classes
#------------------------------------------------------------------------------
{
    package event;
    sub new {
        my ($class, $line) = @_;
        chomp $line;
        my $self = bless {}, $class;
        @{$self}{@event_keys} = split /\t/, $line;
        return $self;
    }
}

{
    package pair;
    sub new {
        my ($class, $key, $value) = @_;
        bless [$key, $value], $class;
    }
    sub key($p) { $p->[0] }
    sub val($p) { $p->[1] }
}

{
    package map;
    sub new {
        my ($class, $event, @pairs) = @_;
        my $self = bless {
            pair => [@pairs],
        }, $class;
        $refs{$event->{anch}} = $self
            if $event->{anch} ne '-';
        $events{Scalar::Util::refaddr($self)} = $event;
        return $self;
    }
    sub add {
        my ($self, $pair) = @_;
        push @{$self->{pair}}, $pair;
    }
    sub pair { $_[0]->{pair} }
}

{
    package seq;
    sub new {
        my ($class, $event, @elems) = @_;
        my $self = bless {
            elem => [@elems],
        }, $class;
        $refs{$event->{anch}} = $self
            if $event->{anch} ne '-';
        $events{Scalar::Util::refaddr($self)} = $event;
        return $self;
    }
    sub add {
        my ($self, $val) = @_;
        push @{$self->{elem}}, $val;
        return $self;
    }
    sub elem { $_[0]->{elem} }
}

{
    package val;
    use overload '""' => sub { $_[0]->{text} };
    my %escapes = (
        'n' => "\n",
        't' => "\t",
        '\\' => '\\',
        '"' => '"',
    );
    sub new {
        my ($class, $event, $text) = @_;
        $text //= $event->{valu};
        $text =~ s/\\([nt\\\"])/$escapes{$1}/g;
        my $self = bless {
            text => $text,
        }, $class;
        delete $event->{valu};
        $refs{$event->{anch}} = $self
            if $event->{anch} ne '-';
        $events{Scalar::Util::refaddr($self)} = $event;
        return $self;
    }
}

{
    package ali;
    sub new {
        my ($class, $event) = @_;
        my $self = bless {
            name => $event->{valu},
        }, $class;
        delete $event->{valu};
        $events{Scalar::Util::refaddr($self)} = $event;
        return $self;
    }
}

1;
