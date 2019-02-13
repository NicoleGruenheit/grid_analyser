#!/usr/bin/perl

use strict;



my($entry,$entry1,$entry2,$count,$count1,$count2,$count3);
my(@div,@div1);
my(%all,%one,%mult,%filtered,%pos);

my $file = $ARGV[0];

open(IN,$file);
	while(<IN>){
		chomp;
		@div = split(/\t/,$_);
		$all{$div[0]}{$div[4]}=$_;
		#print $div[4];
		if($div[3] > 1){
			$mult{$div[0]}{$div[4]}=$_;
		} else {
			$one{$div[0]}{$div[4]}=$_;
		}
	}
close(IN);

%filtered = filter_multiple(\%one,\%mult);
$count = keys %filtered;

#print $count,"\t";
$count1 = keys %one;
$count2 = keys %mult;
$count3 = keys %all;
#print $count1,"\t",$count2,"\t",$count3,"\n";

if($count1 == 0){
	foreach $entry (sort keys %all){
		foreach $entry1 (sort keys %{$all{$entry}}){
			if(!($all{$entry}{$entry1} eq "")){
				@div = split(/\t/,$all{$entry}{$entry1});
				print $all{$entry}{$entry1},"\t",$div[3],"\n";
			}
		}
	}
}else{

	while($count > 0){
		undef(%pos);
		foreach $entry (sort keys %filtered){
			foreach $entry1 (sort keys %{$filtered{$entry}}){
				#print $all{$entry}{$entry1},"\n";
				delete $all{$entry}{$entry1};
			
			}
		}
		foreach $entry (sort keys %all){
			foreach $entry1 (sort keys %{$all{$entry}}){
				@div = split(/\t/,$all{$entry}{$entry1});
				if(!exists $pos{$div[4]}){
					$pos{$div[4]} = 1;
				} else{
					$pos{$div[4]}++;
				}
			}
		}
		undef(%mult);
		undef(%one);
		foreach $entry (sort keys %all){
			foreach $entry1 (sort keys %{$all{$entry}}){
				@div = split(/\t/,$all{$entry}{$entry1});
				if($pos{$div[4]} > 1){
					$mult{$div[0]}{$div[4]}=$_;
				}else{
					$one{$div[0]}{$div[4]}=$_;
				}
			}
		}
		%filtered = filter_multiple(\%one,\%mult);
		$count = keys %filtered;
		#print $count,"\t";
		$count1 = keys %one;
		$count2 = keys %mult;
		$count3 = keys %all;
		#print $count1,"\t",$count2,"\t",$count3,"\n";
	
	}

	foreach $entry (sort keys %all){
		foreach $entry1 (sort keys %{$all{$entry}}){
			if(!($all{$entry}{$entry1} eq "")){
				print $all{$entry}{$entry1},"\t",$pos{$entry1},"\n";
			}
		}
	}
}
sub filter_multiple{
	my $oneref = shift;
	my $multref = shift;
	my %one1 = %{$oneref};
	my %mult1 = %{$multref};
	$count2 = keys %mult1;
	$count3 = keys %one1;
	#print $count2,"\t",$count3,"\n";
	my %filtered1;
	foreach $entry (sort keys %one1){ 
		if(exists $mult1{$entry}){
			foreach $entry1 (sort keys %{$one1{$entry}}){
				foreach $entry2 (sort keys %{$mult1{$entry}}){
					#print $entry,"\t",$entry1,"\t",$entry2,"\n";
					@div = split(/\t/,$mult1{$entry}{$entry2});
					@div1 = split(/\t/,$one1{$entry}{$entry1});
					#print $mult1{$entry}{$entry2},"\n";
					#print $one1{$entry}{$entry2},"\n";
					if($div[12] eq $div1[12] || $div[14] eq $div1[14]){
						$filtered1{$entry}{$entry2}=$mult1{$entry}{$entry2};
					}
				}
			}
		}
	}
	return %filtered1;
}