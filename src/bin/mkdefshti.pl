#!/usr/bin/perl -w
#
# Simple script to convert the LaTeX2HTML generated "labels.pl" to an HTP
# include file.  This allows htp documents to reference labels in the
# LaTeX docmentation via generated URLs.  The URLs are prefixed with the
# path "zanyblue/" as the web site contains the html documents generated
# here with the LaTeX documentation in a "zanyblue" sub-directory and the
# gnathtml generated documents in "zanyblue/ref".
#

use strict;
use FileHandle;

my $SFLOGO = "http://sflogo.sourceforge.net/sflogo.php?group_id=225351&amp;type=11";
my $DOWNLOADURL = "http://sourceforge.net/projects/zanyblue/files/";
my $TRACKERURL = "http://sourceforge.net/tracker/?group_id=225351";
my $PROJECTURL = "http://sourceforge.net/projects/zanyblue/";
my $SVNURL = "https://zanyblue.svn.sourceforge.net/svnroot/zanyblue/trunk";

my $defshti = shift (@ARGV) || Usage ();
my $labelspl = shift (@ARGV) || Usage ();
my $version = shift (@ARGV) || Usage ();
my $status = shift (@ARGV) || Usage ();
my $revision = shift (@ARGV) || Usage ();

$SFLOGO = "/nosuchfile.jpg" if ($revision =~ /[MX]/);

# The LaTeX2HTML generated "labels.pl" assumes the following variables
# are already defined.
our $URL = "zanyblue";
our $key = undef ();
our $nosave = "1";
our %external_labels = ();
our %external_latex_labels = ();
our %noresave = ();

# OK, require the labels.pl file, importing the definitions
require $labelspl;

# OK, write the data loaded to the hti file
my $hti = new FileHandle (">$defshti");
write_general_defs ($hti, $version, $status, $revision);
write_label_defs ($hti, \%external_labels, \%external_latex_labels);
$hti->close();
exit (0);

##############
# mk_version #
##############

sub mk_version {
    my $version = shift;
    my $status = shift;
    my $revision = shift;
    return lc (sprintf ("%s%s-r%s", $version, substr ($status, 0, 1), $revision));
}

sub write_def {
    my $fh = shift;
    my $name = shift;
    my $value = shift;
    $fh->printf ("<set %s=\"%s\" global >\n", $name, $value);
}

######################
# write_general_defs #
######################

sub write_general_defs {
    my $fh = shift;
    my $version = shift;
    my $status = shift;
    my $revision = shift;
    my $version_string = mk_version ($version, $status, $revision);
    my $tarname = "zanyblue-$version_string.tar.gz";
    my $zipname = "zanyblue-$version_string.zip";
    write_def ($fh, "sflogo", $SFLOGO);
    write_def ($fh, "downloadurl", $DOWNLOADURL);
    write_def ($fh, "trackerurl", $TRACKERURL);
    write_def ($fh, "projecturl", $PROJECTURL);
    write_def ($fh, "svnurl", $SVNURL);
    write_def ($fh, "tarname", $tarname);
    write_def ($fh, "downloadtar", "$DOWNLOADURL/$tarname");
    write_def ($fh, "zipname", $zipname);
    write_def ($fh, "version", $version);
    write_def ($fh, "status", $status);
    write_def ($fh, "year", (localtime())[5] + 1900);
}

sub write_label_defs {
    my $fh = shift;
    my $labels = shift;
    my $latex_labels = shift;
    foreach my $key (sort keys (%{$labels})) {
        my $url = $external_labels{$key};
        write_def ($fh, "${key}:url", $url);
    }
    foreach my $key (sort keys (%{$latex_labels})) {
        my $ref = $external_latex_labels{$key};
        write_def ($fh, "${key}:ref", $ref);
    }
}

#########
# Usage #
#########

sub Usage {
    printf (STDERR "Usage: $0 defs.hti labels.pl version status revision\n");
    exit (1);
}
