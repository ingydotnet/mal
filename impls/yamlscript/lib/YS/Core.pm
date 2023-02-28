use strict; use warnings;
use lib '../perl.2/lib';

package YS::Core;

use Core;
use Types;

sub ns {
    +{
        %{Core::ns()},
        'dec' => \&dec,
        'ends-with?' => \&ends_with_q,
        'join' => \&join_,
        'number' => \&number_,
        'range' => \&range,
        'read-file-ys' => \&read_file_ys,
        'read-string' => \&read_string,

        'PPP' => \&PPP,
        'WWW' => \&WWW,
        'XXX' => \&XXX,
        'YYY' => \&YYY,
        'ZZZ' => \&ZZZ,
    }
}

sub dec { number($_[0] - 1) }

sub ends_with_q {
    my ($str, $substr) = @_;
    boolean(
      length($str) >= length($substr) and
      substr($str, 0-length($substr)) eq $substr
    );
}

sub join_ { string(join ${str($_[0])}, map ${str($_)}, @{$_[1]}) }

sub number_ { number("$_[0]" + 0) }

sub range {
    my ($x, $y) = @_;
    if (not defined $y) {
        $y = $x;
        $x = number(0);
    }
    if ($y < $x) {
        list([map number($_), reverse(($y+1)..$x)]);
    } else {
        list([map number($_), $x..($y-1)]);
    }
}

sub read_file_ys {
    my ($file) = @_;
    my $text = slurp($file);
    require YS::YSReader;
    my $ast = YS::YSReader::read_file($text, $file);
    return $ast;
}

sub read_string { YS::Reader->new->read_str(@_) }

1;
