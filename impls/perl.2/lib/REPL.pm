use strict; use warnings;
package REPL;

use ReadLine;
use Reader;
use Eval;
use Printer;
use Env;
use Types;
use Core;

my $prompt = 'user> ';
our $env = Env->new->add(Core::ns);

$env->set('*file*', string($ARGV[0]));
$env->set('*ARGV*', list([map string($_), @ARGV[1..$#ARGV]]));
$env->set('*host-language*', string('perl.2'));
$env->set(eval => sub { Eval::eval($_[0], $env) });

# Define: `not`, 'load-file` and `cond`:
rep('
    (def! not (fn* (a)
        (if a
            false
            true)))');
rep('
    (def! load-file (fn* (f)
      (cond
        (ends-with? f ".mal")
          (eval (read-string (str "(do " (slurp f) "\nnil)")))
        (ends-with? f ".ys")
          (eval (read-file-ys f)))))');
rep(q[
    (defmacro! cond (fn* (& xs)
        (if (> (count xs) 0)
            (list 'if (first xs)
                (if (> (count xs) 1)
                    (nth xs 1)
                    (throw "odd number of forms to cond")
                )
                (cons 'cond (rest (rest xs)))))))]);
rep(q[
    (println
        (str
            "Mal ["
            *host-language*
            "]"))]);
$::x = 1;
sub repl {
    while (defined (my $line = readline($prompt, $env))) {
        try($line) if length $line;
    }
    print "\n";
}

sub try {
    my ($line) = @_;
    eval { print rep("$line") . "\n" };
    if ($@) {
        die $@ if $@ =~ /(^>>|^---\s| via package ")/;
        print "Error: " .
            (ref($@) ? Printer::pr_str($@) : $@) .
            "\n";
    }
}

sub rep {
    my ($str) = @_;
    my $ast = Reader::read_str($str);
    $ast = Eval::eval($ast, $env);
    Printer::pr_str($ast);
}

1;
