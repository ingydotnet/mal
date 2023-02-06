package Eval;

use Mo;

use Types;

sub eval {
    my ($ast, $env) = @_;

    if (not $ast->isa('list')) {
        goto &Eval::eval_ast;
    }
    return $ast if not @$ast;
    my $sym = $ast->[0];
    my $is_sym = ref($sym) eq 'symbol';
    if ($is_sym and $$sym eq 'def!') {
        my (undef, $sym, $val) = @$ast;
        $env->set($$sym, Eval::eval($val, $env));
    } elsif ($is_sym and $$sym eq 'do') {
        my (undef, @do) = @$ast;
        my $last = pop @do;
        eval_ast(list(\@do), $env);
        @_ = ($last, $env);
        goto &Eval::eval;
    } elsif ($is_sym and $$sym eq 'fn*') {
        my (undef, $bind, $form) = @$ast;
        function(
            sub {
                $env = Env->new(
                    outer => $env,
                    binds => $bind,
                    exprs => [@_],
                );
                @_ = ($form, $env);
                goto &Eval::eval;
            }
        );
    } elsif ($is_sym and $$sym eq 'if') {
        my (undef, $cond, $then, $else) = @$ast;
        if (${boolean(Eval::eval($cond, $env))}) {
            @_ = ($then, $env);
        }
        else {
            return nil unless defined $else;
            @_ = ($else, $env);
        }
        goto &Eval::eval;
    } elsif ($is_sym and $$sym eq 'let*') {
        my ($ast, $env) = @_;
        $env = Env->new(outer => $env);
        my (undef, $def, $eval) = @$ast;
        for (my $i = 0; $i < @$def; $i += 2) {
            my ($key, $val) = ($def->[$i], $def->[$i+1]);
            $env->set($$key, Eval::eval($val, $env));
        }
        @_ = ($eval, $env);
        goto &Eval::eval;
    } else {
        my $fn;
        ($fn, @_) = @{eval_ast($ast, $env)};
        goto &$fn;
    }
}

sub eval_ast {
    my ($ast, $env) = @_;

    if ($ast->isa('List')) {
        return ref($ast)->new([ map Eval::eval($_, $env), @$ast ]);
    }
    if ($ast->isa('Map')) {
        return ref($ast)->new([map Eval::eval($_, $env), %$ast]);
    }
    elsif ($ast->isa('symbol')) {
        $env->get($$ast);
    }
    else {
        return $ast;
    }
}

1;
