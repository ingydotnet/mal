use strict; use warnings;
package Core;

use Types;
use Reader;
use Eval;
use Printer;

sub ns {
    {
        '=' => \&equal_to,
        '>' => \&greater_than,
        '>=' => \&greater_equal,
        '<' => \&less_than,
        '<=' => \&less_equal,
        '+' => \&add,
        '-' => \&subtract,
        '*' => \&multiply,
        '/' => \&divide,

        'nil?' => \&nil_q,
        'true?' => \&true_q,
        'false?' => \&false_q,
        'symbol?' => \&symbol_q,

        'atom' => \&atom_,
        'atom?' => \&atom_q,
        'deref' => \&deref,
        'reset!' => \&reset,
        'swap!' => \&swap,

        'list' => \&list_,
        'list?' => \&list_q,
        'cons' => \&cons,
        'concat' => \&concat,
        'vec' => \&vec,
        'nth' => \&nth,
        'first' => \&first,
        'rest' => \&rest,
        'map' => \&map,
        'range' => \&range,

        'count' => \&count,
        'empty?' => \&empty_q,

        'read-string' => \&read_string,
        'slurp' => \&slurp,

        'pr-str' => \&pr_str,
        'str' => \&str,
        'prn' => \&prn,
        'println' => \&println,

        'apply' => \&apply,
        'throw' => \&throw,
    }
}

sub equal_to {
    my ($x, $y) = @_;
    return false
        unless
            ($x->isa('List') and $y->isa('List')) or
            (ref($x) eq ref($y));
    if ($x->isa('List')) {
        return false unless @$x == @$y;
        for (my $i = 0; $i < @$x; $i++) {
            my $bool = equal_to($x->[$i], $y->[$i]);
            return false if "$bool" eq '0';
        }
        return true;
    }
    boolean($$x eq $$y);
}

sub greater_than { $_[0] > $_[1] }
sub greater_equal { $_[0] >= $_[1] }
sub less_than { $_[0] < $_[1] }
sub less_equal { $_[0] <= $_[1] }
sub add { $_[0] + $_[1] }
sub subtract { $_[0] - $_[1] }
sub multiply { $_[0] * $_[1] }
sub divide { $_[0] / $_[1] }

sub nil_q { boolean(ref($_[0]) eq 'nil') }
sub true_q { boolean(ref($_[0]) eq 'boolean' and "$_[0]") }
sub false_q { boolean(ref($_[0]) eq 'boolean' and not "$_[0]") }
sub symbol_q { boolean(ref($_[0]) eq 'symbol') }

sub atom_ { atom(@_) }
sub atom_q { boolean(ref($_[0]) eq 'atom') }
sub deref { $_[0]->[0] }
sub reset { $_[0]->[0] = $_[1] }
sub swap {
    my ($atom, $fn, @args) = @_;
    $atom->[0] = apply($fn, deref($atom), \@args);
}

sub list_ { list([@_]) }
sub list_q { boolean(ref($_[0]) eq 'list') }
sub count { number(ref($_[0]) eq 'nil' ? 0 : scalar @{$_[0]}) }
sub empty_q { boolean(@{$_[0]} == 0) }
sub cons { list([$_[0], @{$_[1]}]) }
sub concat { list([map @$_, @_]) }
sub vec { vector([@{$_[0]}]) }
sub nth {
    my ($list, $index) = @_;
    die "Index '$index' out of range" if $index >= @$list;
    $list->[$index];
}
sub first { ref($_[0]) eq 'nil' ? nil : @{$_[0]} ? $_[0]->[0] : nil }
sub rest {
    my ($list) = @_;
    return list([]) if $list->isa('nil') or not @$list;
    shift @$list;
    list([@$list]);
}
sub map { list([ map apply($_[0], $_, []), @{$_[1]} ]) }
sub range {
    my ($x, $y) = @_;
    if ($y < $x) {
        list([reverse $y..($x+1)]);
    } else {
        list([$x..($y-1)]);
    }
}

sub read_string { Reader::read_str(@_) }
sub slurp {
    my ($file) = @_;
    open my $slurp, '<', "$file" or
        die "Couldn't open '$file' for input";
    local $/;
    string(<$slurp>);
}

sub pr_str { string(join ' ', map Printer::pr_str($_), @_) }
sub str { string(join '', map Printer::pr_str($_, 1), @_) }
sub prn { printf "%s\n", join ' ', map Printer::pr_str($_), @_; nil }
sub println { printf "%s\n", join ' ', map Printer::pr_str($_, 1), @_; nil }

sub apply {
    my ($fn, @args) = @_;
    push @args, @{pop(@args)};
    ref($fn) eq 'CODE' ? $fn->(@args) : Eval::eval($fn->(@args));
}
sub throw {
    die "$_[0]\n";
}

1;
