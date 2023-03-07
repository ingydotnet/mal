use strict; use warnings;
no warnings 'experimental::signatures';
use feature 'signatures';

package YS::YSReader;

use YS::Types;
use YS::Reader;

use IPC::Run qw< run timeout >;
use Scalar::Util 'refaddr';

our %events;
our %functions;
our %refs;
our @event_keys = (qw<
    type
    bpos blin bcol
    epos elin ecol
    anch ytag styl
    valu
>);

#------------------------------------------------------------------------------
# Convert YAMLScript into a Mal Lisp AST
#------------------------------------------------------------------------------
sub read_file {
    my ($text, $file) = @_;

    %events = ();
    %functions = ();
    %refs = ();

    my ($out, $err);
    run [qw< fy-tool --testsuite --tsv-format >],
        $text, \$out, \$err, timeout(5);

    my $events = [ map 'event'->new($_), split /\n/, $out ];

    my $self = bless {
        from => "$file",
        text => "$text",
        events => $events,
    }, __PACKAGE__;

    my $dom = $self->compose;
    my $ast = $self->mal_ast($dom);
    return $ast;
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

sub S { symbol($_[0]) }
sub L { list([@_]) }
sub N { number(@_) }
sub T { string(@_) }

sub DEF { S 'def!' }
sub DO { S 'do' }
sub FN { S 'fn*' }
sub IF { S 'if' }
sub LET { S 'let*' }

# my $A = qr<[a-zA-Z]>;
# my $AN = qr<[a-zA-Z0-9]>;
# my $W = qr<[-a-zA-Z0-9]>;
# my $sym = qr<$A$W+>;
my $sym = qr/[-_\w]+\??/;


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
sub is_single($n) { is_map($n) and pairs($n) == 1 }
sub is_assign($n) {
  is_single($n) and
  text(key(first_pair($n))) =~ /^$sym\s+=$/;
}
sub assert_map($n) { is_map($n) or ZZZ($n) }
sub assert_seq($n) { is_seq($n) or ZZZ($n) }
sub assert_val($n) { is_val($n) or ZZZ($n) }
sub assert_plain($n) { is_plain($n) or ZZZ($n) }
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


sub mal_ast($s, $n) {
    if (is_val($n)) {
        my $text = $n->{text};
        $n->{text} = "(do\n$text\nnil)";
    }
    elsif (is_seq($n)) {
      $n = MAP(
        PAIR( VAL('main()') => SEQ(elems($n)) ),
      );
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
    XXX $n, "Node has no tag" unless $tag;
    my $constructor = "construct_$tag";
    $s->$constructor($n);
}

sub construct_call($s, $n) {
    (my $expr = "$n") =~ s/^($sym)\(/($1 / or die;
    YS::Reader->new->read_str($expr);
}

sub construct_def($s, $n) {
    my ($key, $value) = @$n;
    "$key" =~ /^(\w+)\s*=$/ or die;
    my $sym = S($1);
    my $rhs = $s->construct($value);
    return L(DEF, $sym, $rhs);
}

sub construct_defn($s, $n) {
    my ($key, $value) = @$n;
    text($key) =~ /^($sym)\((.*)\)$/ or die;
    my $name = S($1);
    my $sig = L(map symbol($_), split /\s+/, $2);
    my $defn = L( DEF, S($1), L( FN, L, nil ) );
    my $seq = is_seq($value) ? $value : SEQ($value);
    my @body = map $s->construct($_), @{$seq->elem};
    return L(DEF, $name, L(FN, $sig, @body));
}

sub construct_lisp($s, $n) {
    my $string = "$n";
    return unless $string =~ /^([(\\]|-?\d+$)/;
    $string =~ s/^\\//;
    YS::Reader->new->read_str($string);
}

sub construct_module($s, $n) {
    L(DO, map $s->construct($_), pairs($n));
}

sub construct_string($s, $n) {
    YS::Reader->new->read_str(WWW "$n");
}


#     if (is_map($n) ? construct_map($n) :
#     is_seq($
#     my $ast = do {
#         if (is_map($n)) {
#             try_assign($n) //
#             try_call($n) //
#             try_module($n) //
#             try_hash($n) //
#             XXX $n;
#         }
#         elsif (is_seq($n)) {
#         # try_let($n) //
#             try_do($n) //
#             XXX $n;
#         }
#         elsif (is_val($n)) {
#             try_mal_form($n) //
#             try_scalar_form($n) //
#             XXX $n;
#         }
#         else {
#             XXX $n;
#         }
#     };
#     if (ref($ast) eq 'list' and @$ast == 2 and ref($ast->[0]) eq 'symbol' and "$ast->[0]" eq 'do') {
#         $ast = $ast->[1];
#     }
#     $ast;
# }

sub try_call_map($n) {
    is_single($n) or return;
    my $p = first_pair($n);
    my ($key, $val) = key_val($p);
    "$key" =~ /^$sym\s*:$/ or return;
    $key =~ s/:$//;
    my $op = S($key);
    if (is_val $val) {
        my $ast = construct VAL "( $val\n)";
        unshift @$ast, $op;
        return $ast;
    }
    else {
        XXX $val, "Unknown call value";
    }
}

sub try_let($n) {
    return unless is_assign(first_elem($n));
    my $vars = V();
    my $let = L(LET, $vars);
    my @elems = [@$n];
    while (@elems) {
        last unless is_assign($elems[0]);
        my $elem = shift @elems;
        push @$vars, S(text(key($elem)));
        push @$vars, construct(val($elem));
    }
    map push(@$let, construct($_)), @elems;
    return XXX $let;
}

sub try_do($n) {
    L(DO, map construct($_), @{$n->{elem}});
}

sub try_val_form($n) {
    die;
}

sub try_scalar_form($n) {
    S($n->{text});
}

sub try_assign($n) {
    is_assign($n) or return;
    my ($key, $val) = key_val(first_pair($n));
    (my $sym = "$key") =~ s/\s=$//;
    L( DEF, S($sym), construct($val) );
}

#------------------------------------------------------------------------------
# AST Construction Methods (to Mal Lisp forms)
#------------------------------------------------------------------------------

sub test_ast {
    return YS::Reader->new->read_str('(def! main (fn* () (prn 999)))');
    return L(
        DEF,
        S('main'),
        L(
            FN,
            L,
            L(
                S('prn'),
                N(12345),
            ),
        ),
    );
}

#------------------------------------------------------------------------------
# AST Composer Methods
#------------------------------------------------------------------------------
sub compose {
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

sub tag_error($msg) { die "$msg: '$_'" }

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
    tag_def() or
    tag_defn() or
    XXX $pair, "Unable to implicitly tag this map pair.";
}

sub tag_seq {}

sub tag_val {
    if (is_plain($_) or e_tag($_) eq '!') {
        is_key($_) or
        tag_lisp() or
        tag_call() or
        tag_json() or
        tag_error("Unresolvable plain scalar");
    } else {
        $_->{ytag} = 'string';
    }
}

sub tag_call {
    return if is_key($_);
    $_->{ytag} = 'call' if /^$sym\(.*\)$/;
}

sub tag_def {
    $_->{ytag} = 'def' if /^$sym\s*=$/;
}

sub tag_defn {
    $_->{ytag} = 'defn' if /^$sym\((.*)\)$/;
}

sub tag_istr {
}

sub tag_json {
    $_->{ytag} =
        /^(true|false)$/ ? 'boolean' :
        /^-?\d+$/ ? 'int' :
        /^-?\d+\.\d*$/ ? 'float' :
        /^null$/ ? 'null' :
        return;
}

sub tag_lisp {
    $_->{ytag} = 'lisp' if /^\(/;
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
    sub val($p) { XXX $p->[1] }
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
