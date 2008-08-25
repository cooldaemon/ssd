#!/usr/bin/env perl

use strict;
use warnings;

use version; our $VERSION = qv('0.0.1');

use Carp;
use English qw(-no_match_vars);

use Path::Class;

use FindBin qw($Bin);
use lib "$Bin/lib";
use SingleStorageClient;

my $ssc = SingleStorageClient->new;

my $res = $ssc->get('test');
print 'get code: ', $res->code, "\n" or croak $OS_ERROR;

$res = $ssc->set(test => 'test message.');
print 'set code: ', $res->code, "\n" or croak $OS_ERROR;

$res = $ssc->head('test');
print 'head code: ', $res->code, "\n" or croak $OS_ERROR;

$res = $ssc->get('test');
print 'get content: ', $res->content, "\n" or croak $OS_ERROR;

$res = $ssc->delete('test');
print 'del code: ', $res->code, "\n" or croak $OS_ERROR;

$res = $ssc->set(test => '日本語');
print 'set code: ', $res->code, "\n" or croak $OS_ERROR;

$res = $ssc->get('test');
print 'get code: ',    $res->code,    "\n" or croak $OS_ERROR;
print 'get content: ', $res->content, "\n" or croak $OS_ERROR;

$res = $ssc->delete('test');
print 'del code: ', $res->code, "\n" or croak $OS_ERROR;

$res = $ssc->set('test.jpg' => read_file(), 'image/jpeg');

sub read_file {
    my $fh = Path::Class::File->new($Bin, 'test.jpg')->openr
      or croak $OS_ERROR;

    my $content;
    while ($fh->read(my $buff, 1024)) {
        $content .= $buff;
    }

    $fh->close or croak $OS_ERROR;

    return $content;
}
