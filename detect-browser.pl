#!/usr/bin/perl

use HTTP::BrowserDetect();
use JSON;

my $user_agent_string = $ARGV[0];
if (!$user_agent_string) {
    print "Usage: detect-browser.pl <user-agent>\n";
    exit;
}

my $ua = HTTP::BrowserDetect->new($user_agent_string);

my %bhash =
    ("browser" => $ua->browser_string,
     "OS" => $ua->os,
     "mobile" => $ua->mobile,
     "tablet" => $ua->tablet,
     "robot" => $ua->robot,
     "lib" => $ua->lib);
print encode_json \%bhash, "\n";

