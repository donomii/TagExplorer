#thumbnail all files in file display (if possible)
#present option to sort tags by number or by alphabetical
#method to rename files when the user realises they are misclassified
my $tags = {};
my $whole_names = {};
my $file_metadata = {};
my $tagscore = {};
my @whole_names = ();
my @tags = ();
my @blocked_tags = ();
my @recent_tags = ();
my $files = {};
my $file;
my $r;
my @stop_words = split /,/, "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";
use File::Find;
use Data::Dumper;
use strict;

sub is_in { my $search = shift; scalar (grep { $_ eq $search } @_)}
sub filter_stop_words {grep {!is_in(lc($_), @stop_words)} @_}

sub addtags {my $file = shift;
		#print $file, "\n";
		foreach my $tag (@_) {
			my $t = $tag;
			$t=~ s/s$//;
			#print "	+$t\n";
			push @{$tags->{$t}}, $file;
			$tagscore->{$t}++;
			$files->{$file}->{$t}++}

}

sub maketags {
my @tags=();
my $longname = shift;
chomp $longname;
$longname =~ m!/([^/]+)$!;
my $shortname = $1;

		$shortname =~ s/\.(...)$//;
		#FIXME We should try to detect decimal points and not change them
		
		if ($1){push @tags, $1;}
		$shortname =~ s/\.|_/ /g;;
		push @tags, split /\-+|\[+|\]+/, $shortname;
		push @tags, split /\-+| +|_+/, $shortname;
		push @tags, split /\/+/, $longname;
		push @whole_names, split /\/+/, $longname;
		return @tags;
}
        print "Scanning for files, please wait.  This may take a long
time...\n";
           #find(\&wanted, 'C:/Users/user/Dropbox');
		#find(\&wanted, 'z:/torrents/Completed Torrents');
		my $path = $ARGV[0];
		warn "Scanning $path";
		find(\&wanted, $path);
        my $numfiles;
           sub wanted {
        $numfiles++;  
        #print $File::Find::name, "\n";
		my $f = $_;
		chomp $f;
		chomp $File::Find::name;
		$file_metadata->{$File::Find::name}->{name}=$f;
		addtags($File::Find::name, maketags($File::Find::name));
		}
        print "Scanned $numfiles files.\n";

$tags->{''}=[];
$files->{''}={};

sub makedeletetaghref {
s~/+~/~g;
return "<a href=\"/deletetag/$_\">+$_</a>";
}
sub makedeleteblockedtaghref {
s~/+~/~g;
return "<a href=\"/deleteblockedtag/$_\">-$_</a>";
}
sub makeselectedtags {
join(",", map { makedeletetaghref($_) } @tags) .",".
join(",", map {makedeleteblockedtaghref($_) } @blocked_tags );
}
sub makefilehref {
s~/+~/~g;
my $file = $_;
my $meta = $file_metadata->{$file};
return "<li><a href=\"/file/$_\">$meta->{name}</a></li>";
}
sub makedownloadlink {
#s~/+~/~g;
return "<a href=\"/downloadfile/$_[0]\">$_[0]</a>";
}
sub makeimgsrc {
#s~/+~/~g;
return "<img src=\"/downloadfile/$_[0]\" />";
}
sub maketaghref {
s~/+~/~g;
return "<a href=\"/tag/$_\">$_($tagscore->{$_})</a><a href=\"/blocktag/$_\">-</a>";
}
sub wrap {
my $stuff = shift;
return "<HTML><style>#Recent_Tags { float: right;}</style><BODY>$stuff</BODY></HTML>";
}

sub maketagsbyletter {
  my $letter = shift;
  my @tags = grep { m/^$letter/} keys %$tagscore;
}

sub box {
my $title = shift;my $content = shift;;
my $id = $title;
$id =~ s/\s+/_/g;
return "<div id=\"$id\" style=\"border:2px solid black;\">$title:<div>$content</div></div>"
}

sub listbox {
my $title = shift;my $content = shift;;
my $id = $title;
$id =~ s/\s+/_/g;
return "<div id=\"$id\" style=\"border:2px solid black;\">$title:<div><ul>$content</ul></div></div>"
}

sub embed_video {
my $vid_link = shift;
$vid_link="/downloadfile/".$vid_link;
return qq!

<video width="640" height="360" controls autoplay>
	<source src="$vid_link"  type="video/mp4" />
	<source src="$vid_link"  type="video/ogg" />
	<object width="640" height="360" type="application/x-shockwave-flash" data="__FLASH__.SWF">
		<param name="movie" value="__FLASH__.SWF" />
		<param name="flashvars" value="autostart=true&amp;controlbar=over&amp;image=__POSTER__.JPG&amp;file=__VIDEO__.MP4" />
		
	</object>
</video>
<p>	<strong>Download Video:</strong>
	Closed Format:	<a href="$vid_link">"MP4"</a>
	Open Format:	<a href="$vid_link">"Ogg"</a>
</p>
!;
}

sub delete_array {my $arr1 = shift;my $arr2=shift;
my $h1={}; foreach(@$arr1){$h1->{$_}=1;}
my $h2={}; foreach(@$arr2){$h2->{$_}=1;}
foreach(@$arr2){delete($h1->{$_})};
return keys(%$h1); }
sub wrapfiles { return listbox("Files",$_[0])}
sub wraptags { return box("Tags",$_[0]) }
sub wraprelatedtags { return box("Related Tags",$_[0]) }
sub wraprecenttags { return box("Recent Tags",join(",", map(&maketaghref, @_))) }
sub makefiledisplay {
my $fname = shift;
my $head = `head  -c 1024 "$fname"`;
return "<div>".makeimgsrc($fname).
#embed_video($fname).
"
File: $fname
<pre>$head</pre>
".makedownloadlink($fname)." ".
join(", ", map(maketaghref, maketags($fname)))."</div>";
}

sub filter_suitable {
   return  filter_stop_words(@_);
   }
   
sub actually_take100 {@_[0..99]}
sub take100 {
  return   grep { length($_) >2 } actually_take100 filter_stop_words grep { length($_) >2 } map { s/^\s+//;$_ } @_;
  #return  @list[0..99];
}

sub array_sub {
	my ($first, $second) = @_;
	my %diff = map { ("$_" => 1) }  @$second;
	return  grep {!$diff{"$_"}} @$first ;
} 

sub process_path {
	my $p = shift;
	my $c = shift;
	$p = URI::Encode::uri_decode($p);
	if ($p=~ m/\/favicon.ico$/){return ""}
	$p =~ s/^\/(.+?)\///;
			print "File is: $file\n";
			my $action = $1;
			print "Action is: $action\n";
			if ($action eq 'file') {$file = $p;}
			if ($action eq 'downloadfile') {
			  print "Sending $file\n";
			  fork || do{$c->send_file_response($file);
			  exit;
			};
			die "Skipping file closes\n";;
			return undef;
$r->content_type('video/mp4');
$r->content_type('application/octet-stream');
open FILE, "<$file";
binmode FILE, "binary";
 return sub {my $temp;  read FILE,$temp,1024*1024; return $temp;};
return undef;}
			if ($action eq 'tag') {push @tags,$p;unshift @recent_tags, $p;undef $file;}
			if ($action eq 'blocktag') {push @blocked_tags,$p;undef $file;}
			if ($action eq 'deletetag') {@tags = grep { $_ ne $p} @tags;undef $file;}
			if ($action eq 'deleteblockedtag') {@blocked_tags = grep { $_ ne $p} @blocked_tags;undef $file;}
			
			#print "Answering @tags\n";
			my $accum = makeselectedtags();
			my @selected_files =  Set::Intersection::get_intersection(map {$tags->{$_}} @tags);
				@selected_files = grep 
					      {my $file = $_; my $blocked =  scalar ( grep {my $tag = $_;  is_in($tag, @blocked_tags) } keys %{$files->{$file}} ); if ($blocked) {print "File: $file\n";print "Blocked: $blocked\n";} !$blocked }
						@selected_files;
			
			if (@tags ) {
				if ($file) {$accum .= makefiledisplay($file);
						#$accum .= map maketaghref maketags($file);
}
				else {	
					$accum .= wrapfiles (join("",  map (makefilehref, @selected_files)))
					;
				}
			}
if (scalar(@selected_files)==0){$accum .="No matching files.  Try removing a tag by clicking on it";}
				$accum.= wraprecenttags(@recent_tags[0..9]);
				my $related_tags = {};
				
				map {	my $file = $_;
					foreach my $tag (keys %{$files->{$file}}){ 
						$related_tags->{$tag}+=$files->{$file}->{$tag}
					}
				} @selected_files;
				my @rt = keys %$related_tags;
				my @global_tags = sort {$a cmp $b} take100 sort { $tagscore->{$b} <=> $tagscore->{$a} }  keys %$tagscore;
				#my @related_tags = array_sub(\@rt, \@global_tags);
				my @related_tags = @rt;
				#print "Related tags: @related_tags\n";
				#$accum.=wraprelatedtags(join(", ",take100 map (maketaghref, sort { $tagscore->{$b} <=> $tagscore->{$a} } @related_tags)));
				$accum.=wraprelatedtags(
				join(", ",  
				  map (maketaghref, 
				    sort {$a cmp $b}  
				      take100 ( 
					sort { $related_tags->{$b} <=> $related_tags->{$a} } 
					  grep { $tagscore->{$_} > 1} 
					  @related_tags))));
				$accum.=wraptags(join(", ", map (maketaghref, @global_tags)));
				my @global_names = keys %$tagscore;
				$accum.=wraptags(join(", ", map (maketaghref,
				  sort {$a cmp $b}  
				      take100 ( 
					sort { $tagscore->{$b} <=> $tagscore->{$a} }  
					  delete_array(\@global_names, \@whole_names)))));
return(wrap($accum));


}


$SIG{'PIPE'} = 'IGNORE';
         use HTTP::Daemon;
         use HTTP::Status;
	use HTTP::Response;
	use LWP::UserAgent;
	

         my $d = HTTP::Daemon->new(LocalPort=>61120, Reuse=>1) || die;
         print "Please contact me at: <URL:", $d->url, ">\n";
         while (1){ eval {
		
		my $c = $d->accept;
             my $req = $c->get_request; 
                 if ($req->method eq 'GET'){
			  my $ua = LWP::UserAgent->new;
		        $r = $ua->request($req);
			$r->clear;
			$r->content_type('text/html');
			$r->code(200);
			$r->header('Connection'=>'close');

			my $p = $req->uri->path;  
			print "Path is: $p\n";

			my $content = process_path($p, $c);
			$r->content($content) if $content;
			#
			$c->send_response($r) if $content;
			#
			undef $r;
                 }
                 else {
                     $c->send_error(RC_FORBIDDEN)
                 }
		undef $req;
             $c->close;
             undef($c);
		print "Purged vars\n";
         };print $@.$!."\n"}




package Set::Intersection;

use warnings;
use strict;

=head1 NAME

Set::Intersection - provides an API to get intersection (of set theory) of ARRAYs.

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.02';


=head1 SYNOPSIS

  use Set::Intersection;
  
  my @arr1 = qw/3 1 4 1 5 9/;
  my @arr2 = qw/1 7 3 2 0 5/;
  my @intersection = get_intersection(\@arr1, \@arr2);
  # got (1, 3, 5) in @intersection

=head1 EXPORT

get_intersection

=cut

require Exporter;
our @ISA = qw/Exporter/;

our @EXPORT = qw/get_intersection/;

=head1 FUNCTIONS

=head2 get_intersection [\%options,] [\@ARRAY[, \@ARRAY[, ...]]]

Returns intersection set (as LIST) of all ARRAYs.

=over 1

=item The result LIST is uniqized and unordered.

=item If no ARRAYs passed, the result LIST is empty.

=item If only one ARRAY passed, the result LIST is same as the passed. (elements won't be uniqized nor order-changed)

=item If you have undef in any LIST, you'll be warned.

=back

=over 1

=item %options

=over 2

=item -preordered => BOOLEAN

=over 3

=item To reduce calculation time, get_intersection sorts ARRAYs
      by their length before calculating intersections.

=item This option tells that order of ARRAYs are well done,
      and calculation of intersection will be based on left most ARRAY.

=back

=back 

=back

=cut

my %_default_opts = (
  -preordered => 0,
);

sub get_intersection
{
  my %opts;
  if ( ref($_[0]) =~ m{^HASH} ) {
    %opts = (%_default_opts, %{$_[0]});
    shift;
  }

  my @arrs = @_;
  return () if !@arrs;
  return @{$arrs[0]} if @arrs == 1;

  @arrs = sort { @$a <=> @$b } @arrs if !$opts{-preordered};

  my $head = shift @arrs;

  _intersection($head, @arrs);
}

sub _intersection
{
  my ($head, @left) = @_;

  my %h = map { $_ => undef } @$head;
  for my $l ( @left ) {
    %h = map { $_ => undef } grep { exists $h{$_} } @$l;
  }
  keys %h;
}

=head1 SEE ALSO

List::Compare, Set::Object

=head1 AUTHOR

turugina, C<< <turugina at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-list-intersection at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Set-Intersection>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Set::Intersection


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Set-Intersection>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Set-Intersection>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Set-Intersection>

=item * Search CPAN

L<http://search.cpan.org/dist/Set-Intersection/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT & LICENSE

Copyright 2009 turugina, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut

1; # End of Set::Intersection
package URI::Encode;

use 5.008001;
use warnings;
use strict;
use Carp;
use Encode qw();

our $VERSION = 0.03;

## Exporter
use base qw(Exporter);
our @EXPORT_OK = qw(uri_encode uri_decode);

## OOP Intrerface

# Constructor
sub new {
    my $class = shift;
    my $self = bless {}, $class;
    return $self;
}

# Encode
sub encode {
    my ( $self, $url, $encode_reserved ) = @_;
    return unless $url;

    # Encode URL into UTF-8
    $url = Encode::encode( 'utf-8-strict', $url );

    # Create character map
    my %map = map { chr($_) => sprintf( "%%%02X", $_ ) } ( 0 ... 255 );

    # Create Regex
    my $reserved =
      qr{([^a-zA-Z0-9\-\_\.\~\!\*\'\(\)\;\:\@\&\=\+\$\,\/\?\%\#\[\]])}x;
    my $unreserved = qr{([^a-zA-Z0-9\Q-_.~\E])}x;

    # Percent Encode URL
    if ($encode_reserved) {
        $url =~ s/$unreserved/$map{$1}/gx;
    }
    else {
        $url =~ s/$reserved/$map{$1}/gx;
    }

    return $url;
}

# Decode
sub decode {
    my ( $shift, $url ) = @_;
    return unless $url;

    # Character map
    my %map = map { sprintf( "%02X", $_ ) => chr($_) } ( 0 ... 255 );

    # Decode percent encoding
    $url =~ s/%([a-fA-F0-9]{2})/$map{$1}/gx;
    return $url;
}

## Traditional Interface

# Encode
sub uri_encode {
    my ( $url, $flag ) = @_;
    my $uri = URI::Encode->new();
    return $uri->encode( $url, $flag );
}

# Decode
sub uri_decode {
    my ($url) = @_;
    my $uri = URI::Encode->new();
    return $uri->decode($url);
}

## Done
1;
__END__

=pod

=head1 NAME

URI::Encode - Simple URI Encoding/Decoding

=head1 VERSION

This document describes URI::Encode version 0.03

=head1 SYNOPSIS

	## OO Interface
	use URI::Encode;
	my $uri = URI::Encode->new();
	my $encoded = $uri->encode($url);
	my $decoded = $uri->decode($encoded);

	## Using exported functions
	use URI::Encode qw(uri_encode uri_decode);
	my $encoded = uri_encode($url);
	my $decoded = uri_decode($url);

=head1 DESCRIPTION

This modules provides simple URI (Percent) encoding/decoding. This module
borrows from L<URI::Escape>, but 'tinier'

=head1 METHODS

=head2 new()

Creates a new object, no argumetns are required

	my $encoder = URI::Encode->new();

=head2 encode($url, $including_reserved)

This method encodes the URL provided. The method does not encode any
L</"Reserved Characters"> unless C<$including_reserved> is true. The $url
provided is first converted into UTF-8 before percent encoding.

	$uri->encode("http://perl.com/foo bar");      # http://perl.com/foo%20bar
	$uri->encode("http://perl.com/foo bar", 1);   # http%3A%2F%2Fperl.com%2Ffoo%20bar

=head2 decode($url)

This method decodes a 'percent' encoded URL. If you had encoded the URL using
this module (or any other method), chances are that the URL was converted to
UTF-8 before 'percent' encoding. Be sure to check the format and convert back
if required.

	$uri->decode("http%3A%2F%2Fperl.com%2Ffoo%20bar"); # "http://perl.com/foo bar"

=head1 EXPORTED FUNCTIONS

The following functions are exported upon request. This provides a non-OOP
interface

=head2 uri_encode($url, $including_reserved)

See L</encode($url, $including_reserved)>

=head2 uri_decode($url)

See L</decode($url)>

=head1 CHARACTER CLASSES

=head2 Reserved Characters

The following characters are considered as reserved (L<RFC
3986|http://tools.ietf.org/html/rfc3986>). They will be encoded only if requested.

	 ! * ' ( ) ; : @ & = + $ , / ? % # [ ]

=head2 Unreserved Characters

The following characters are considered as Unreserved. They will not be encoded

	a-z
	A-Z
	0-9
	- _ . ~

=head1 DEPENDENCIES

L<Encode>

=head1 ACKNOWLEDGEMENTS

Gisle Aas for L<URI::Escape>

David Nicol for L<Tie::UrlEncoder>

=head1 SEE ALSO

L<RFC 3986|http://tools.ietf.org/html/rfc3986>

L<URI::Escape>

L<URI::Escape::XS>

L<URI::Escape::JavaScript>

L<Tie::UrlEncoder>

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests to C<bug-uri-encode@rt.cpan.org>, or
through the web interface at L<http://rt.cpan.org>.

=head1 AUTHOR

Mithun Ayachit  C<< <mithun@cpan.org> >>

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2010, Mithun Ayachit C<< <mithun@cpan.org> >>. All rights
reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlartistic>.

=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE
SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE
STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE
SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
PERFORMANCE OF THE SOFTWARE IS WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE,
YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY
COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE
SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE SOFTWARE (INCLUDING BUT NOT LIMITED TO
LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR
THIRD PARTIES OR A FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES.
