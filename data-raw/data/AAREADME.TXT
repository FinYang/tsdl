pablo1.dat

-time series matrix with year in col 1, 1662-1986
-these are the original **six** tree-ring sites, given to me by Dr. David Meko
 of the University of Arizona's tree-ring laboratory.  He used these data to extend
 the Gila River flow and published the results in the AWRA's Water Resources Bulletin,
 Oct. 1995 (?) issue.  

NTFLW94A.ANN 
-tabulation of 6 unregulated (natural) annual river flow series in 1000 acre-feet
   col. 2 is the Snake River near Moran, Wyoming; 1904-1994
   col. 3 is the Snake River near Heise, Idaho; 1911-1994
   col. 4 is the Boise River near Twin Springs; 1912-1994
   col. 5 is the Salmon River near Whitebird, Idaho; 1912-1994
   col. 6 is the Bruneau River near Hot Springs, Idaho; 1912-1994 (partly estimated)
   col. 7 is the Willow Creek near Ririe, Idaho; 1917-1994 (partly estimated)

NATFLO94.CFS
-tabulation of 6 unregulated monthly river flow series in cfs
   (same rivers and same order as of file NTFLW94A.ANN)

NATFLO94.TAB
-tabulation of 6 unregulated monthly river flow series in 1000 acre-feet
   (same rivers and same order as of file NTFLW94A.ANN)
   
The rest of the files are annual tree-ring series in two formats for each series
  or most of the tree-ring series I have in this disk comes in pair.

   Example:
      - AZ510.CRN is the original data file downloaded from the International
                  Tree-Ring Data Base (ITRDB) of Boulder, Colorado.
                  As you may note, AZ stands for Arizona.   The first four lines
                  of the file contains important information.  It has the name of
                  the tree, the location of the site in latitude and longitude,
                  the site elevation in either meters or feet, the developer or
                  author of the data series, the beginning year and ending year
                  of the series and the date the series was finished or reported.
                  This file is one of several files that can be downloaded from ITRDb through
                  ftp.ngdc.noaa.gov/pub/treering/softwares/SWUSA.EXE; 
                  SWUSA.EXE is compressed but I unpacked it within the
                  PC MS DOS platform.  The other files in that location are
                  ASIA.EXE, SPACIFIC.EXE, CANADA.EXE, EUROPE.EXE, SAMERICA.EXE,
                  MIDUSA.EXE, NEUSA.EXE, SOUTHUSA.EXE, and NWUSA.EXE.  There are
                  several *.CRN files that are not yet contained in these compressed
                  files and they are found at ftp.ngdc.noaa.gov/pub/treering/chronologies. 
                  
      - AZ510.DAT is the reformated file of two columns; the first column is the
                  year and the second column is the relative or normalized 
                  tree-ring width in dimensionless unit.  The relative tree-ring
                  size is the same value found from the file *.CRN;  I had to
                  retabulate the data in this two-column format since MathCad
                  could not read a series of values without space(s) between them.
                  I could not figure out how to make it read the data.  Up to
                  this point in time, I am still not sure that Mathcad can read
                  the *.CRN files.

                  I had to write a short Fortran routine to retabulate the tree-
                  ring series in this two column format.  I would welcome any
                  suggestion on how to make Mathcad read the *.CRN tree-ring
                  series.  I haven't told Mathsoft, Inc. about my problem but
                  I am not sure they would help me any way.

          NOTE:
                  I terminated each of the *.dat series with a few 9.9 or 9.99 
                  at the end.  I use this value since a small tree-ring value
                  like 0.1 or less becomes possible at the end although unlikely. 
                  I wanted to be sure that I was really reading the data series
                  not beyond the last one.
         
                  If one can only figure out how to make MathCad read *.CRN,
                  then *.DAT tree-ring files in this disk can be deleted. 
                  That would result into a large space savings in the disk for
                  better use or storage.


Pablo G. Baldazo may be reached by:

phone: (208) 362-0507 home
       (208) 362-0507 home fax

       (208) 327-7987 work
       (208) 327-7866 fax
       pbaldazo@idwr.state.id.us  (no assurance on this one)

A letter to Pablo G. Baldazo on this address would receive prompt
      response:
                   10325 West Victory
                   Boise, Idaho 83709