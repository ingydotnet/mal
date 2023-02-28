use strict; use warnings;
use lib '../perl.2/lib';

package YS::REPL;
use base 'REPL';

use YS::Core;
use YS::Reader;
use YS::Types;

use constant core_class => 'YS::Core';
use constant reader_class => 'YS::Reader';

sub init {
    my $self = shift;
    $self->SUPER::init(@_);
    $self->rep('
      (def! load-file (fn* (f)
        (cond
          (ends-with? f ".mal")
            (eval
              (read-string
                (str
                  "(do "
                  (slurp f)
                  "\nnil)")))
          (ends-with? f ".ys")
            (eval
              (read-file-ys f)))))');
}

1;
