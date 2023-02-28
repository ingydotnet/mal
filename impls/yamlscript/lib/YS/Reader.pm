use strict; use warnings;
package YS::Reader;

use base 'Reader';

use YS::Types;

sub read_symbol {
    my ($self, $symbol) = @_;
    $symbol =~ s/^(def|defmacro)$/$1\!/;
    $symbol =~ s/^(catch|fn|let|try)$/$1\*/;
    symbol($symbol);
}

1;
