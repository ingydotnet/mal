package Env;

use Types;

use Mo qw< build default >;

has outer => ( default => undef, lazy => 0);
has stash => ( default => {}, lazy => 0 );
has binds => [];
has exprs => [];

sub BUILD {
    my ($self) = @_;
    my $binds = $self->binds;
    my $exprs = $self->exprs;
    while (@$binds) {
        $self->set(shift(@$binds), shift(@$exprs));
    }
    delete $self->{binds};
    delete $self->{exprs};
}

sub set {
    my ($self, $key, $val) = @_;
    $self->{stash}{$key} = $val;
}

sub find {
    my ($self, $key) = @_;
    while ($self) {
        if (defined $self->{stash}{$key}) {
            return $self;
        }
        $self = $self->{outer};
    }
    die "Symbol '$key' not found in Env";
}

sub get {
    my ($self, $key) = @_;
    $self->find($key)->{stash}{$key};
}

1;
