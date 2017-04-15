#!/usr/bin/env perl

use strict;
use warnings;

$| = 1;

my $POWSUP_LOCATION = "/sys/class/power_supply";

sub getBatDirs {
    my @acSources = glob ($POWSUP_LOCATION . "/*");
    my @ret = ();
    foreach my $sourcePath (@acSources) {
        my $idx = rindex($sourcePath, "/");
        my $sourceName = substr($sourcePath, $idx+1);

        # Skip AC source
        next if $sourceName eq "AC";
        my $percent = getFile($sourcePath . "/capacity");
        chomp($percent);
        my $state = getFile($sourcePath . "/status");
        chomp($state);
        push @ret, { 'pc' => $percent, 'n' => $sourceName, 'st' => $state };
    }
    return @ret;
}

sub getFile {
    open my $fh, '<', $_[0] or die "Can't open file $_[0]: $!";
    my $file_content = do { local $/; <$fh> };
    close $fh;
    return $file_content;
}

while (1) {
    my @batStat = getBatDirs();
    foreach my $bat (@batStat) {
        my $statusChar = substr($bat->{st},0,1);
        my $buf =  $bat->{n} . " " . $bat->{pc} . "%";
        print " | ".$buf;
        print " $statusChar" if $statusChar ne "U";
    }
    print "\n";
    sleep 5;
}
