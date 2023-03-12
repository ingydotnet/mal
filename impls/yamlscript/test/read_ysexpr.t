use Test::More;

use YAML::PP;

use lib './lib', '../perl.2/lib';

use YS::YSReader;
use Printer;

my $tests = <<'...';
- - ()
  - ()

- - (foo)
  - (foo)

- - (foo "bar" 42)
  - (foo "bar" 42)

- - "('\nfoo)"
  - (foo)

- - (' foo)
  - (foo)

- - ('foo)
  - ((quote foo))

- - foo()
  - (foo)

- - foo(bar(baz))
  - (foo (bar baz))

- - foo(x - 1)
  - (foo (- x 1))

- - (x - 1)
  - (- x 1)

- - (' x - 1)
  - (x - 1)

- - (x >= 1)
  - (>= x 1)

- - (x > 1)
  - (> x 1)

- - (x = 1)
  - (= x 1)

- - (x == 1)
  - (== x 1)

- - (x and y)
  - (and x y)

- - ((x < 1) and (y > 2))
  - (and (< x 1) (> y 2))

- - ((x < 1) and (> y 2))
  - (and (< x 1) (> y 2))

- - (foo ())
  - (foo ())

- - (foo = 'bar)
  - (= foo (quote bar))
...

for my $test (@{YAML::PP::Load($tests)}) {
    my ($expr, $want) = @$test;
    my $got = Printer::pr_str(YS::YSReader::read_ysexpr($expr));
    my $label = sprintf "%-25s => %s", $expr, $want;
    $label =~ s/\n/␤/g;
    is $got, $want, $label;
}

done_testing;
