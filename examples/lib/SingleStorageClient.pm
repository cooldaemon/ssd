package SingleStorageClient;

use strict;
use warnings;

use version; our $VERSION = qv('0.0.1');

use Carp;
use English qw(-no_match_vars);

use LWP::UserAgent;

use Readonly;
Readonly my $URI => 'http://localhost:8080/%s';

use base qw(Class::Accessor::Fast);
__PACKAGE__->mk_ro_accessors(qw(uri));

for my $sub_name (qw(get head delete)) {
    no strict 'refs';    ## no critic
    *{__PACKAGE__ . q{::} . $sub_name} = sub {
        my $self = shift;
        my ($key) = @_;
        return $self->_request(uc $sub_name => $key);
    };
}

sub new {
    my $class = shift;
    return bless {
        uri => $URI,
        ua  => LWP::UserAgent->new(parse_head => 0),
        @_
    }, $class;
}

sub set {    ## no critic
    my $self = shift;
    my ($key, $value, $type,) = @_;
    $type ||= 'text/plain';

    my $req = HTTP::Request->new(
        PUT => sprintf($self->uri, $key)    ## no critic
    );
    $req->content_type($type);
    $req->content($value);

    return $self->{ua}->request($req);
}

sub _request {
    my $self = shift;
    my ($method, $key,) = @_;

    return $self->{ua}->request(
        HTTP::Request->new(
            $method => sprintf($self->uri, $key)    ## no critic
        )
    );
}

1;
