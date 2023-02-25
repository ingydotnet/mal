use strict; use warnings;
no warnings 'experimental::signatures';
use feature 'signatures';

package YSReader;

use Types;
use Reader;

use IPC::Open2;
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

use Reader;
#------------------------------------------------------------------------------
# Convert YAMLScript into a Mal Lisp AST
#------------------------------------------------------------------------------
sub read_file {
    my ($text, $file) = @_;

    %events = ();
    %functions = ();
    %refs = ();

    my $pid = open2(\*IN, \*OUT,
        qw< fy-tool --testsuite --tsv-format >
    ) or die "Can't open pipe to fy-tool";
    print OUT "$text";
    close OUT;
    waitpid $pid, 0;
    die "Error parsing '$file' (rc = $?):\n$@"
        unless $? == 0;

    my $events = [ map 'event'->new($_), <IN> ];
    close IN;

    my $self = bless {
        from => "$file",
        text => "$text",
        events => $events,
    }, __PACKAGE__;

    my $dom = $self->compose_node;
    my $ast = mal_ast($dom);
    return $ast;
}

#------------------------------------------------------------------------------
# AST Implicit Typing Methods
#------------------------------------------------------------------------------

my $E_GROUP = 'event'->new("=xxx\t-1\t-1\t-1\t-1\t-1\t-1\t-\t-\t-\t-");
my $E_PLAIN = 'event'->new("=xxx\t-1\t-1\t-1\t-1\t-1\t-1\t-\t-\t:\t-");
my $E_QUOTE = 'event'->new("=xxx\t-1\t-1\t-1\t-1\t-1\t-1\t-\t-\t'\t-");
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


use Carp qw< confess >;
sub error($m) { confess "YS Error: $m\n" }
sub event($n) { $events{refaddr($n)} }
sub style($n) { event($n)->{styl} }
sub is_map($n) { ref($n) eq 'map' }
sub is_seq($n) { ref($n) eq 'seq' }
sub is_val($n) { ref($n) eq 'val' }
sub is_plain($n) { is_val($n) and style($n) eq ':' }
sub is_assign($n) {
    return unless is_map($n);
    @{pairs($n)} == 1 and text(key(first_pair($n))) =~ /^$sym\s+=$/;
}
sub assert_map($n) { is_map($n) or ZZZ($n) }
sub assert_seq($n) { is_seq($n) or ZZZ($n) }
sub assert_val($n) { is_val($n) or ZZZ($n) }
sub assert_plain($n) { is_plain($n) or ZZZ($n) }
sub assert_pair($n) { XXX $n }
sub assert_elems($n) { assert_seq($n); @{$n->elem} > 0 or ZZZ($n) }
sub assert_pairs($n) { assert_map($n); @$n > 0 or ZZZ($n) }
sub pairs($n) { assert_map($n); @{$n->pair} }
sub elems($n) { assert_seq($n); @{$n->elem} }
sub get_key_val($n) { assert_map($n); @{$n->pair} }
sub key($p) { assert_pair($p); $p->key }
sub val($p) { assert_pair($p); $p->val }
sub text($v) { assert_val($v); $v->{text} }
sub first_elem($n) { assert_elems($n); (elems($n))[0] }
sub first_pair($n) { assert_pairs($n); (pairs($n))[0] }


sub mal_ast($n) {
    if (is_val($n)) {
        my $text = $n->{text};
        $n->{text} = "(do\n$text\nnil)";
    }
    elsif (is_seq($n)) {
      $n = MAP(
        VAL('main()') => MAP(
          VAL('do:') => SEQ(elems($n)),
        )
      );
    }
    my $ast = get_form($n);

    if (has_main($ast)) {
        $ast = L(
            DO,
            $ast,
            L( S('main') ),
        );
    }

    return $ast;
}

sub get_form($n) {
    if (is_map($n)) {
        # try_call($n) //
        try_module($n) //
        try_hash($n) //
        XXX $n;
    }
    elsif (is_seq($n)) {
        try_let($n) //
        try_do($n) //
        XXX $n;
    }
    elsif (is_val($n)) {
        try_mal_form($n) //
        try_scalar_form($n) //
        XXX $n;
    }
    else {
        XXX $n;
    }
}

sub try_module($n) {
    my @pairs = pairs($n);
    for my $p (@pairs) {
        is_plain($p->[0]) or return;
    }
    L(
        DO,
        map {
            try_defn($_) //
            try_def($_) //
            XXX $_;
        } @pairs,
    );
}

sub try_call($n) {
    @{pairs($n)} == 1 or return;
    get_key_val($1) =~ /^$sym:/ or return;
    XXX $n;
    return;
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
        push @$vars, get_form(val($elem));
    }
    map push(@$let, get_form($_)), @elems;
    return XXX $let;
}

sub try_do($n) {
    L(DO, map get_form($_), @{$n->{elem}});
}

sub try_val_form($n) {
    die;
}

sub try_mal_form($n) {
    my ($t) = $n->{text};
    return unless $t =~ /^[(\\]/;
    $t =~ s/^\\//;
    Reader::read_str($t);
}

sub try_scalar_form($n) {
    $n->{text};
}

sub try_defn($n) {
    my ($key, $value) = @$n;
    text($key) =~ /^($sym)\((.*)\)$/ or return;
    my $name = S($1);
    my $sig = L(map symbol($_), split /\s+/, $2);
    my $defn = L( DEF, S($1), L( FN, L, nil ) );
    my $seq = is_seq($value) ? $value : SEQ($value);
    my @body =
        try_let($seq) //
        map get_form($_), @{$seq->elem};
    return L(DEF, $name, L(FN, $sig, @body));
}

sub try_def($n) {
    my ($key, $value) = @$n;
    $key->{text} =~ /^(\w+)\s*=$/ or return;
    my $sym = S($1);
    my $rhs = get_form($value);
    return L(DEF, $sym, $rhs);
}

sub has_main($ast) {
    return 0 unless ref($ast) eq 'list';
    for my $node (@$ast) {
        return 1 if
            ref($node) eq 'list' and
            @$node == 3 and
            ref($node->[0]) eq 'symbol' and
            $node->[0] eq 'def!' and
            ref($node->[1]) eq 'symbol' and
            $node->[1] eq 'main' and
            ref($node->[2]) eq 'list' and
            @{$node->[2]} >= 3 and
            $node->[2][0] eq 'fn*' and
            1;
    }
    return 0;
}

#------------------------------------------------------------------------------
# AST Construction Methods (to Mal Lisp forms)
#------------------------------------------------------------------------------

sub test_ast {
    return Reader::read_str('(def! main (fn* () (prn 999)))');
    return L(
        DEF,
        S('main'),
        L(
            S('fn*'),
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
sub compose_node {
    my ($self) = @_;
    my $events = $self->{events};
    while (@$events) {
        my $event = shift(@$events);
        if ($event->{type} =~ /^[+=](map|seq|val|ali)$/) {
            my $composer = "compose_$1";
            return $self->$composer($event);
        }
    }
}

sub compose_map {
    my ($self, $event) = @_;
    my $map = 'map'->new($event);;
    my $events = $self->{events};
    while (@$events) {
        shift(@$events), return $map if $events->[0]{type} eq '-map';
        my $key = $self->compose_node;
        my $val = $self->compose_node;
        my $pair = 'pair'->new($key, $val);
        $map->add($pair);
    }
    XXX $map;
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
    XXX $seq;
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
    use overload '""' => sub($n) { $n->{text} };
    my %escapes = (
        'n' => "\n",
        't' => "\t",
        '\\' => '\\',
        '"' => '"',
    );
    sub new {
        my ($class, $event) = @_;
        my $text = $event->{valu};
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
