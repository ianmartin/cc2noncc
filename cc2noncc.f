c
c CC2NONCC
c      
c Author: Jim Ray
c         U.S. National Geodetic Survey
c         NOAA N/NGS6, SSMC3/8117
c         1315 East-West Hwy
c         Silver Spring, MD  20910
c         USA
c         tel +1-301-713-2850 x112
c         fax +1-301-713-4475
c         e-mail jimr@ngs.noaa.gov
c               
c Maintainer: Ignacio (Nacho) Romero
c             European Space Agency / European Space Operation Centre
c             SAC s.l. @ Navigation Support Office
c             ESA / ESOC / OPS-GN
c             Robert-Bosch-str 5
c             64293, Darmstadt 
c	      Germany
c             nng.esoc.esa.de
c             www.canaryadvancedsolutions.com
c             e-mail: Ignacio.Romero@esa.int
c                     nacho@canaryadvancedsolutions.com
c
c   DESCRIPTION:
c
c   This utility is used to convert a RINEX file from a receiver
c   which uses the cross-correlation technique (e.g., the AOA
c   TurboRogue receivers) to pseudorange observables compatible
c   with the modern Y-codeless pseudorange tracking used in such
c   receivers as Ashtech, AOA Benchmark, AOA SNR-12 ACT, etc.
c
c   Specifically, the C1 and P2' (equivalent to C1 + (P2-P1))
c   pseudorange observables of cross-correlation receivers are
c   replaced by
c      C1  -->  C1 + f(i)
c      P2' -->  P2'+ f(i)
c   where f(i) are empirically-determined long-term average values
c   <P1 - C1>i as a function of GPS satellite PRNi.  The set of f(i)
c   is given in a DATA statement below.  This transformation accounts
c   for the different satellite-based biases between cross-correlation
c   pseudorange observables (C1, P2') and the observables (P1, P2)
c   reported by modern Y-codeless receivers.  In this way, a mixed
c   network of receiver types can be used together consistently.
c
c   In addition, a few models of non-cross-correlator receivers report
c   the C1 observable rather than P1.  For these types, only the C1
c   is replaced by
c      C1  -->  C1 + f(i)
c   and the P2 observable is left unchanged.
c
c   The (P1 - C1) biases vary from satellite to satellite but tend to
c   be relatively constant in time.  Using the (P1, P2) pseudorange
c   pair together with the (C1, P2') pair is not consistent and will
c   lead to different biases affecting the satellite clock estimates.
c   Note that since the biases change from satelite to satellite
c   (unlike for phase observables) they will not be eliminated by
c   double differencing.
c
c   The coding here is modelled on the utility "clockprep" by Jeff
c   Freymueller.  This routine performs the reverse function of the
c   "noncc2cc" routine, although the logic and implementation are
c   different in applying a constant correction for each satellite
c   rather than a correction based on measurements at each epoch.
c
c   RUNSTRING PARAMETERS
c
c   cc2noncc <infile> <outfile> <biasfile> [type]
c
c   <infile>      Input RINEX file name (required)
c   <outfile>     Output RINEX file name (required)
c   <biasfile>    file of historic P1-C1 values & receiver types
c                 (required)
c   [type]        force corrections to be applied for these two
c                 types:  (optional but [biasfile] must be given)
c                   C1P2  = C1 and P2 both are to be changed
c                   C1    = only C1 is to be changed
c
c   Only the most recent P1-C1 bias values are used when <biasfile> is
c   not provided.  By supplying the external bias file, the appropriate
c   bias values for any epoch in the past may be used, not just the most
c   recent set which is the default.  The receiver types in the <biasfile>
c   override the default types in the source code.
c
c   When run with no runstring parameters, help info is returned.
c
c   COMPILE & LOAD
c
c   Recommended Fortran compile & load options for HP-UX 11.11:
c
c   f90 -O cc2noncc.f -o cc2noncc
c
c   where:
c
c   -O    default optimization = +O2 level
c
c   FORMATS SUPPORTED:
c   RINEX version 1 and 2
c         
c   KNOWN BUGS/LIMITATIONS/WARNING:
c   1) up to 9 observation types are supported
c   2) up to 40 PRN numbers (01-40) are supported
c   3) only specific receiver types are handled thus the RINEX header
c      must be reliable; the supported types must be hardcoded based
c      on the variable rec_type
c   4) up to 64 event records (COMMENTS) within observation records are
c      supported
c   5) no modification will be made for a file which has already been
c      converted, as indicated by an inserted COMMENT record in the
c      RINEX header (unless the [type] runstring option is used to
c      force corrections to be applied)
c   6) The operation of Trimble receivers (4000SSE, 4000SSI, 4700, and
c      5700) depends on the A/S state (for each individual satellite);
c      in each case, they switch to true P1, P2 tracking when A/S is off.
c      It is assumed here that A/S is always on.
c   7) Bias values can only be applied to data collected starting 02 April
c      2000 (using the p1c1bias.hist file as an external bias file).  For
c      data from earlier periods, the user may modify the external bias
c      file to include additional bias values from some other source.
c   8) The external routine "getarg" is platform and compiler dependent.
c      Modifications may be needed for other operating systems.
c   9) Up to 100 receiver names of each type are permitted in the optional
c      external bias file.
c  10) According to Etienne Orliac <isxejo@nottingham.ac.uk>, under the g77
c      linux compiler from Redhat 9 it is necessary to change the line:
c        endheader=(fmt_vers.eq.1.and.(line .eq. ' '.or. line.eq.' 
c'))
c      in the skip_header subroutine to (uncommented):
c        endheader=(fmt_vers.eq.1.and.(line .eq. ' '.or. line.eq.'
c    +              '))
c      and likewise the same change in the copy_header subroutine.
c
c   MODIFICATIONS:
c  
c   JimR 10Jan2000: original version written
c   JimR 27Jan2000: add "AOA ICS-4000Z    " to list of receivers to mod
c   JimR 06Mar2000: use announced set of bias values
c   JimR 09Mar2000: change biases to latest set from Ron Muellerschoen (JPL)
c   JimR 08Jun2000: change biases to latest set from David Jefferson (JPL);
c                   add new PRN20; drop decommissioned PRN14
c   JimR 04Jan2001: change biases to the (P1-C1) biases estimated and posted
c                   automatically by Stefan Schaer (AIUB) at
c                   http://www.aiub.unibe.ch/ionosphere.html; refer to IGS
c                   Mail #2827 (09 May 2000).  Set to version 2.0.
c   JimR 05Mar2001: update biases to monthly averages for Feb 2001 from Stefan
c                   Schaer (AIUB) on 05 Mar 2001; add PRN18. [vers 2.1]
c   JimR 19Mar2001: add "TRIMBLE 4700" to list of receivers to mod [vers 2.2]
c   LimVu30May2001: remove dynamic format statements and fortran runstring
c                   parameters for consistency with LINUX g77 [ver 2.3]
c   JimR 02Jan2002: add "TRIMBLE 5700" to list of receivers to mod;
c                   update biases to monthly averages for Nov 2002 from Stefan
c                   Schaer (AIUB) on 05 Dec 2001; fixed new comments when no
c                   previous comments present.  [vers 2.4]
c   JimR 29Jan2002: create new class of receiver types to modify -- these are
c                   not cross-correlators but they report C1 instead of P1;
c                   only the C1 observable is modified (e.g., Leicas);
c                   in addition the "TRIMBLE 5700" receiver is moved from
c                   cross-correlator receivers to this new class, based on
c                   new information.
c                   New Limitation (6) added to note the dependence on whether
c                   A/S is on/off; here A/S is assumed to always be on.
c                   [vers 3.0]
c   JimR 19Dec2002: the "TRIMBLE 4700" receiver was moved from the cross-
c                   correlator class to the new non-cross-correlators that
c                   report C1 instead of P1 -- see IGS Mail #3887 by Stefan
c                   Schaer (17 May 2002).  [vers 3.1]
c   JimR 19Dec2002: added "LEICA RS500 " to the new class of C1/P2 receivers
c                   per email from Angie Moore (18 Dec 2002) based on
c                   confirmation from the CODE group.  [vers 3.2]
c   JimR 26Feb2003: added runstring option to use external file of historic
c                   P1-C1 bias values (nominally p1c1bias.hist file);
c                   added runstring option to force corrections to be applied;
c                   updated biases to 30-day average DCB solution ending
c                   21 Feb 2003 from Stefan Schaer (AIUB) on 26 Feb 2003.
c                   [vers 4.0]
c   JimR 04Mar2003: fixed potential logic bug in reading external bias file;
c                   updated to use Fortran 90 compiler, which required
c                   replacement of fdate with date_and_time; note that +U77
c                   compiler option no longer needed.  [vers 4.1]
c   JimR 17Apr2003: modified to read receiver types from external bias file,
c                   if provided, rather than use default receiver types
c                   coded below; added "TRIMBLE MS750" to the class of C1/P2
c                   receivers and "TOPCON GP-DX1" to the class of cross-
c                   correlators.  [vers 4.2]
c   JimR 05May2003: updated biases to monthly average DCB solution for April
c                   2003 from Stefan Schaer (AIUB) on 04 May 2003.  [vers 4.3]
c   JimR 08May2003: changed INT(AINT(bias(i)*1.d3)) syntax to more reliable
c                   NINT((bias(i)*1.d3)) in writing out bias values to
c                   output rinex headers.  [ver 4.4]
c   JimR 04Feb2004: updated biases to monthly average DCB solution for January
c                   2004 from Stefan Schaer (AIUB) on 04 Feb 2004.  [vers 4.5]
c   JimR 10May2004: updated biases to monthly average DCB solution for April
c                   2004 from Stefan Schaer (AIUB) on 04 May 2004; also added
c                   "LEICA SR520 ", "LEICA SR530 ", "NOV MILLEN-RT2",
c                   "NOV MILLEN-STD", and "NOV MILLEN-STDW" to the class of
c                   C1/P2 receivers. [vers 4.6]
c   JimR 04Aug2004: updated biases to monthly average DCB solution for July
c                   2004 from Stefan Schaer (AIUB) on 04 Aug 2004; also added
c                   write to stderr of version if no runstrings given.
c                   [vers 4.7]
c   JimR 05Aug2004: correct COMMENT for C1-only case to indicate conversion
c                   to P1 instead of P2.  [vers 4.8 -- not released yet]
c   JimR 04Jan2005: updated biases to monthly average DCB solution for Dec.
c                   2004 from Stefan Schaer (AIUB) on 04 Jan 2005
c                   [vers 4.8]
c   JimR 14Mar2005: added "NOV EURO4-1.00-222" to class of C1/P2 receivers
c                   [vers 4.9]
c   JimR 23Nov2005: FINAL UPDATE BY ME. In future, users must maintain for
c                   themselves.  To force this, use of external <biasfile> is
c                   now required instead of optional.  No bias values are
c                   provided in the non-commented source code anymore.
c                   [vers 5.0]
c   Nacho 11Mar2008: Update to RINEX 2.11, keeping GLONASS measurements intact.
c                    [vers 6.0]
c   Nacho 29Aug2008: updated program to read and correct files with more than 9
c		     measurement types, up to 15 measurements types are 
c		     supported now.  Increased the number of satellites 
c		     supported per epoch from 24 to 99. [vers 6.1]
c   Nacho 15Sep2008: updated program to skip over COMMENT lines in the middle of
c		     the RINEX files correctly (as when splicing files together). 
c		     [ver 6.2]
c
c   Nacho 27Apr2011: updated program to handle 3 lines of observations
c		     [ver 6.3]
c
      program cc2noncc
c
      implicit none
c
c     ... Input/Output units
c
      integer        stdin, stdout, stderr
c
      parameter      (stdout=6)
      parameter      (stdin=5)
      parameter      (stderr=6)
c
c     ... other local variables
c
      logical        eof, err, mods, help, fixed, c1only
      character*132  infile, outfile, biasfile
      character*4    force
      character*80   eventrecs(64)
      character*60   cbias(5)
      character*60   version, log_line, comment
c     character*25   tstring
      character*8    cdate
      character*10   ctime
      character*5    czone
      character*1    char(99)
      character*20   rec_type
      character*20   c1p2list(100), c1onlylist(100)
      character*4    marker
      character*2    obs_type(18)
      character*1    lli(18,99), snr(18,99),charwarn(40)
      integer        irec, iu, ou, bu, ierr, itime(5), flag, numsat,
     +               prn(24), nobs, sec, msec,
     +               i, j, fmt_vers, first_obs(3), iarray(8)
      integer        c1f, p1f, p2f
      integer        line_n, nwarn, prnwarn(40)
      integer        nc1p2, nc1only
      real*8         obs(18,99), clockerr, bias(40)
c
c     ... Average (P1-C1) biases by PRN number used for corrections.
c         This set of biases is based on 835 station-days of data
c         from a diverse set of IGS sites collected between 21 December
c         1999 and 03 March 2000.  They have been renormalized to zero
c         mean across the constellation in order to leave the net
c         receiver clock bias unchanged for the older receiver types
c         which are modified.  The estimated long-term accuracy is
c         roughly 5 cm, mostly due to intrinsic temporal variations.
c
c         Units are meters!
c         The value -9.999d9 flags PRNs with no determination.
c
c     data bias / -0.003d0, -0.282d0,  0.020d0,  0.465d0, -0.251d0,
c    +             0.167d0, -0.232d0, -0.148d0,  0.102d0, -0.361d0,
c    +             0.022d0, -9.999d9,  0.581d0,  0.151d0, -0.209d0,
c    +            -0.350d0, -0.301d0,  0.066d0,  0.100d0, -9.999d9,
c    +            -0.057d0, -0.473d0, -0.268d0,  0.052d0,  0.249d0,
c    +             0.386d0, -0.024d0, -9.999d9,  0.258d0,  0.554d0,
c    +            -0.214d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Average (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Ron Muellerschoen (JPL)
c         based on 8 days of 1-second data from 14 Ashtech Z12
c         receivers maintained in stable conditions.  The raw data
c         were collected between 3-Jan-2000 16:00 and 11-Jan-2000 21:00.
c         These biases have been renormalized to zero mean across the
c         constellation in order to leave the net receiver clock bias
c         unchanged for the older receiver types.
c
c         Units are meters!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 02 April 2000 -- IGS Mail #2744 :
c
ch 2000 04 02
c     data bias / -0.067d0, -0.308d0,  0.052d0,  0.458d0, -0.195d0,
c    +             0.172d0, -0.296d0, -0.240d0,  0.117d0, -0.465d0,
c    +            -0.035d0, -9.999d9,  0.526d0,  0.172d0, -0.297d0,
c    +            -0.202d0, -0.266d0,  0.052d0,  0.070d0, -9.999d9,
c    +            -0.084d0, -0.469d0, -0.147d0,  0.132d0,  0.242d0,
c    +             0.433d0, -0.007d0, -9.999d9,  0.296d0,  0.541d0,
c    +            -0.183d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Average (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by David Jefferson (JPL)
c         based on Ashtech Z12 data only (e-mail on 08 June 2000).  The
c         data were collected after the launch of PRN20/SVN51, which has
c         added, and the decommissioning of PRN14, which has been dropped.
c         These biases have been renormalized to zero mean across the
c         constellation in order to leave the net receiver clock bias
c         unchanged for the older receiver types.
c
c         Units are meters!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 18 June 2000 -- IGS Mail #2879 :
c
ch 2000 06 18
c     data bias / -0.076d0, -0.261d0,  0.074d0,  0.508d0, -0.174d0,
c    +             0.209d0, -0.272d0, -0.157d0,  0.124d0, -0.464d0,
c    +            -0.011d0, -9.999d9,  0.549d0, -9.999d9, -0.316d0,
c    +            -0.185d0, -0.228d0,  0.083d0,  0.081d0, -0.256d0,
c    +            -0.137d0, -0.450d0, -0.148d0,  0.180d0,  0.206d0,
c    +             0.463d0,  0.004d0, -9.999d9,  0.281d0,  0.560d0,
c    +            -0.187d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring.  They are direct
c         estimates from a mixed network of cross-correlation style and
c         non-cross-correlation receivers; see IGS Mail #2827 (09 May 2000)
c         and the web site at http://www.aiub.unibe.ch/ionosphere.html.
c         These values are a 7-day moving average of the daily estimates.
c         The particular set used here were those posted on 26 Dec 2000.
c
c         Units are NANOSECONDS!  (THIS IS A CHANGE FROM METERS BEFORE!!!)
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 14 January 2001 -- IGS Mail #3160 :
c
ch 2001 01 14
c     data bias /  0.222d0, -0.546d0,  0.042d0,  1.294d0, -0.798d0,
c    +             0.625d0, -0.523d0, -0.193d0,  0.048d0, -1.002d0,
c    +            -0.329d0, -9.999d9,  1.545d0, -0.409d0, -0.755d0,
c    +            -9.999d9, -0.522d0, -9.999d9,  0.582d0, -0.958d0,
c    +            -0.172d0, -1.374d0, -1.018d0,  0.459d0,  0.775d0,
c    +             1.077d0,  0.213d0, -0.144d0,  0.611d0,  1.745d0,
c    +            -0.496d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring.  They are direct
c         estimates from a mixed network of cross-correlation style and
c         non-cross-correlation receivers; see IGS Mail #2827 (09 May 2000)
c         and the web site at http://www.aiub.unibe.ch/ionosphere.html.
c         These values are the monthly average of the daily estimates for
c         Feb. 2001.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 18 March 2001 -- IGS Mail #3220 :
c
ch 2001 03 18
c     data bias /  0.039d0, -0.469d0,  0.104d0,  1.655d0, -0.634d0,
c    +             0.681d0, -0.192d0, -0.309d0,  0.317d0, -1.003d0,
c    +            -0.161d0, -9.999d9,  1.376d0, -0.391d0, -0.238d0,
c    +            -9.999d9, -0.695d0, -0.242d0,  0.541d0, -1.114d0,
c    +            -0.176d0, -1.664d0, -1.033d0,  0.708d0,  0.373d0,
c    +             1.022d0,  0.123d0, -0.323d0,  0.511d0,  1.621d0,
c    +            -0.426d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring.  They are direct
c         estimates from a mixed network of cross-correlation style and
c         non-cross-correlation receivers; see IGS Mail #2827 (09 May 2000)
c         and the web site at http://www.aiub.unibe.ch/ionosphere.html.
c         These values are the monthly average of the daily estimates for
c         Nov. 2001.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 20 January 2002 -- IGS Mail #3674 :
c
ch 2002 01 20
c     data bias /  0.377d0, -0.611d0,  0.343d0,  1.109d0, -0.738d0,
c    +             0.329d0, -0.557d0, -0.061d0,  0.172d0, -1.226d0,
c    +             0.229d0, -9.999d9,  1.519d0, -0.279d0, -0.751d0,
c    +            -9.999d9, -0.722d0, -0.666d0, -9.999d9, -0.953d0,
c    +            -0.088d0, -0.626d0, -1.308d0,  0.167d0,  0.791d0,
c    +             0.888d0,  0.367d0, -0.217d0,  0.760d0,  2.015d0,
c    +            -0.261d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring.  They are direct
c         estimates from a mixed network of cross-correlation style and
c         non-cross-correlation receivers; see IGS Mail #2827 (09 May
c         2000), IGS Mail #3212 (23 Feb 2001), and the web site at
c         http://www.aiub.unibe.ch/ionosphere.html.  These values are the
c         30-day averages of the daily estimates for the period ending
c         21 Feb 2003 (retrieved 26 Feb 2003).
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 02 March 2003 -- IGS Mail #4279 :
c
ch 2003 03 02
c     data bias / -0.067d0, -0.944d0,  0.106d0,  1.508d0, -0.802d0,
c    +             0.645d0, -0.916d0, -0.514d0,  0.380d0, -1.480d0,
c    +             0.692d0, -9.999d9,  1.503d0,  0.289d0, -0.830d0,
c    +            -0.561d0, -0.595d0,  0.084d0, -9.999d9, -1.084d0,
c    +            -9.999d9, -1.609d0, -0.740d0,  0.347d0,  0.720d0,
c    +             1.223d0, -0.023d0, -0.113d0,  0.867d0,  2.211d0,
c    +            -0.296d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring.  They are direct
c         estimates from a mixed network of cross-correlation style and
c         non-cross-correlation receivers; see IGS Mail #2827 (09 May
c         2000), IGS Mail #3212 (23 Feb 2001), and the web site at
c         http://www.aiub.unibe.ch/ionosphere.html.  These values are the
c         monthly averages of the daily estimates for April 2003,
c         generated on 04 May 2003.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 18 May 2003 -- IGS Mail #4366 :
c
ch 2003 05 18
c     data bias / -0.107d0, -1.062d0,  0.149d0,  1.535d0, -0.890d0,
c    +             0.596d0, -0.618d0, -0.513d0,  0.320d0, -1.658d0,
c    +             0.755d0, -9.999d9,  1.559d0,  0.427d0, -0.947d0,
c    +            -0.285d0, -0.858d0,  0.085d0, -9.999d9, -1.019d0,
c    +            -0.267d0, -1.422d0, -0.745d0,  0.345d0,  0.611d0,
c    +             1.322d0,  0.005d0, -0.096d0,  0.956d0,  2.065d0,
c    +            -0.244d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring.  They are direct
c         estimates from a mixed network of cross-correlation style and
c         non-cross-correlation receivers; see IGS Mail #2827 (09 May
c         2000), IGS Mail #3212 (23 Feb 2001), and the web site at
c         http://www.aiub.unibe.ch/ionosphere.html.  These values are the
c         monthly averages of the daily estimates for January 2004,
c         generated on 04 February 2003.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 04 Feb 2004 -- IGS Mail #4825 :
c
ch 2004 02 04
c     data bias / -0.052d0, -1.096d0,  0.015d0,  1.383d0, -0.821d0,
c    +             0.607d0, -0.942d0, -0.603d0,  0.392d0, -1.400d0,
c    +             0.487d0, -9.999d9,  1.435d0,  0.180d0, -0.926d0,
c    +            -0.517d0, -0.811d0, -0.066d0, -9.999d9, -1.109d0,
c    +            -0.437d0,  0.374d0, -0.426d0,  0.337d0,  0.569d0,
c    +             1.293d0, -0.062d0, -0.276d0,  0.785d0,  2.019d0,
c    +            -0.333d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring; see above.  These
c         values are the monthly averages of the daily estimates for
c         April 2004 (generated 04 May 2004).  Note that since the last
c         update the constellation has changed with the decommissioning of
c         the old PRN23 on 13 Feb 2004, the end of life of PRN02 on 23 Feb
c         2004 (though not officially decommissioned yet), and the new
c         IIR-11 satellite (PRN19) launched on 20 Mar 2004.  Besides the
c         bias changes for PRN02, PRN19, and PRN23, the only other having
c         a bias change greater than 0.2 ns was PRN22, which became
c         operational in late Dec 2003.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 16 May 2004 -- IGS Mail #4937 :
c
ch 2004 05 16
c     data bias / -0.076d0, -9.999d9,  0.014d0,  1.473d0, -0.872d0,
c    +             0.565d0, -0.809d0, -0.563d0,  0.327d0, -1.579d0,
c    +             0.609d0, -9.999d9,  1.600d0,  0.311d0, -1.040d0,
c    +            -0.460d0, -0.915d0,  0.014d0, -2.410d0, -1.084d0,
c    +            -0.298d0,  0.652d0, -9.999d9,  0.258d0,  0.552d0,
c    +             1.247d0, -0.020d0, -0.162d0,  0.949d0,  2.082d0,
c    +            -0.366d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring; see above.  These
c         values are the monthly averages of the daily estimates for
c         July 2004 (generated 04 Aug 2004).  Note that since the last
c         update the constellation has changed with the launch of a new
c         IIR-12 satellite (SVN60/PRN23) on 23 Jun 2004.  Besides the
c         bias changes for PRN02, PRN19, and PRN23, the only other having
c         a bias change greater than 0.2 ns was PRN22, which became
c         operational in late Dec 2003.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 15 Aug 2004 -- IGS Mail #4987 :
c
ch 2004 08 15
c     data bias / -0.053d0, -9.999d9, -0.011d0,  1.450d0, -0.944d0,
c    +             0.538d0, -1.228d0, -0.243d0,  0.401d0, -1.582d0,
c    +             0.553d0, -9.999d9,  1.621d0,  0.441d0, -0.948d0,
c    +            -0.410d0, -0.835d0,  0.063d0, -2.323d0, -1.074d0,
c    +            -0.345d0,  0.605d0, -0.225d0,  0.289d0,  0.570d0,
c    +             1.277d0,  0.001d0, -0.204d0,  0.969d0,  2.095d0,
c    +            -0.449d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring; see above.  These
c         values are the monthly averages of the daily estimates for
c         December 2004 (generated 04 Jan 2005).  Note that since the last
c         update the constellation has changed with the launch of a new
c         IIR-13 satellite (SVN61/PRN02) on 06 Nov 2004.  Besides the
c         new bias for PRN02, which is close to zero, the only other
c         having a bias change greater than 0.2 ns is PRN07 (+0.32ns),
c         which has historically been unstable.
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 16 Jan 2005 -- IGS Mail #5078 :
c
ch 2005 01 16
c     data bias / -0.028d0, -0.061d0,  0.077d0,  1.334d0, -0.929d0,
c    +             0.664d0, -0.912d0, -0.335d0,  0.529d0, -1.567d0,
c    +             0.535d0, -9.999d9,  1.541d0,  0.335d0, -1.057d0,
c    +            -0.419d0, -0.906d0,  0.098d0, -2.269d0, -1.105d0,
c    +            -0.346d0,  0.579d0, -0.221d0,  0.152d0,  0.735d0,
c    +             1.247d0, -0.018d0, -0.205d0,  0.842d0,  2.017d0,
c    +            -0.307d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Estimated (P1-C1) biases by PRN number used for corrections.
c         This set of biases was determined by Stefan Schaer (AIUB) as
c         a byproduct of his ionosphere monitoring; see above.  These
c         values below are the 30-day averages of the daily estimates
c         ending on 19 Nov 2005.  Note that since the last update in
c         Jan 2005 the constellation has changed with the launch of the
c         new IIR-M1 satellite (SVN53/PRN17) on 26 Sep 2005 and the
c         decommissioning of SVN31/PRN31 on 25 Oct 2005.  Besides the new
c         bias for PRN17 the only other having a bias change greater than
c         0.3 ns is PRN23 (+0.35ns).
c
c         Units are NANOSECONDS!
c         The value -9.999d9 flags PRNs with no determination.
c
c         For data collected starting 03 Oct 2005 -- IGS Mail #5260 :
c
ch 2005 10 03
c     data bias / -0.233d0, -0.033d0, -0.163d0,  1.234d0, -0.944d0,
c    +             0.444d0, -1.132d0, -0.416d0,  0.366d0, -1.651d0,
c    +             0.591d0, -9.999d9,  1.529d0,  0.108d0, -1.326d0,
c    +            -0.525d0,  1.391d0, -0.012d0, -2.026d0, -1.228d0,
c    +            -0.392d0,  0.532d0,  0.130d0, -0.069d0,  0.575d0,
c    +             1.083d0, -0.189d0, -0.232d0,  0.643d0,  2.016d0,
c    +            -0.073d0, -9.999d9, -9.999d9, -9.999d9, -9.999d9,
c    +            -9.999d9, -9.999d9, -9.999d9, -9.999d9, -9.999d9  /
c
c     ... Read the command line arguments
c
      help = .false.
      infile = '                                                       '
      outfile = '                                                      '
      biasfile = '                                                     '
      force = '    '
c
c     ... The indexing for runstring parameters depends on compiler and
c         operating system.  For some systems, N=1 returns the program
c         name so runstring parameters are N+1, ....
c
      call GETARG(1, infile)
      call GETARG(2, outfile)
      call GETARG(3, biasfile)
      if (infile(1:1) .eq. ' ')  help = .true.
      if (outfile(1:1) .eq. ' ') help = .true.
      if (biasfile(1:1) .eq. ' ') help = .true.
      if (biasfile(1:1) .ne. ' ') then
        call GETARG(4, force)
        if (force(1:2) .ne. '  ') then
          if (force(1:4) .eq. 'c1p1') force(1:4) = 'C1P1'
          if (force(1:4) .eq. 'c1  ') force(1:4) = 'C1  '
          if (force(1:4) .ne. 'C1P1' .and.
     +        force(1:4) .ne. 'C1  ') then
            write(stderr,*) 'ERROR: attempt to force invalid   '
            write(stderr,*) '       correction type            '
            help = .true.
          endif
        endif
      endif
c
c     ... Write the version string, log line, and comment line
c
      call blnkstrng (version, 60)
      call blnkstrng (comment, 60)
c      version = 'CC2nonCC jimr Version 5.0, 23 Nov 2005'
c      version = 'CC2nonCC ESOC Version 6.0, 25 Mar 2008'
c      version = 'CC2nonCC ESOC Version 6.1, 29 Aug 2008'
c      version = 'CC2nonCC ESOC Version 6.2, 15 Sep 2008'
      version = 'CC2nonCC ESOC Version 6.3, 27 Apr 2011'
      write(stderr,*) version
      write(stderr,*) ' '
c
c     ... Replace f77 routine fdate with f90 routine for system time
c
c     call fdate(tstring)
c     log_line = 'CC2nonCC executed '//tstring//' '
c
      call date_and_time(cdate, ctime, czone, iarray)
      call blnkstrng (log_line, 60)
      log_line = 'CC2nonCC executed '//cdate//' '//ctime//'     '
c     write(stderr,*) log_line
c
c     ... Print out help if required or requested
c
c'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
c
      if (help) then
         write(stderr,*)
     +   'cc2noncc <infile> <outfile> <biasfile> [type]'
         write(stderr,*) ' '
         write(stderr,*)
     +   '<infile>      Input RINEX file name (required)'
         write(stderr,*)
     +   '<outfile>     Output RINEX file name (required)'
         write(stderr,*)
     +   '<biasfile>    file of historic P1-C1 values & receiver types'
         write(stderr,*)
     +   '              (required)                      '
         write(stderr,*)
     +   '[type]        force corrections to be applied for these two'
         write(stderr,*)
     +   '              types:  (optional but <biasfile> must be given)'
         write(stderr,*)
     +   '                C1P2  = C1 and P2 both are to be changed'
         write(stderr,*)
     +   '                C1    = only C1 is to be changed'
         write(stderr,*) ' '
         write(stderr,*)
     +   'DESCRIPTION:'
         write(stderr,*) ' '
         write(stderr,*)
     +   'This utility is used to convert a RINEX file from a receiver'
         write(stderr,*)
     +   'which uses the cross-correlation technique (e.g., the AOA '
         write(stderr,*)
     +   'TurboRogue receivers) to pseudorange observables compatible'
         write(stderr,*)
     +   'with the modern Y-codeless pseudorange tracking used in such'
         write(stderr,*)
     +   'receivers as Ashtech, AOA Benchmark, AOA SNR-12 ACT, etc. '
         write(stderr,*) ' '
         write(stderr,*)
     +   'Specifically, the C1 and P2'' (equivalent to C1 + (P2-P1))'
         write(stderr,*)
     +   'pseudorange observables of cross-correlation receivers are'
         write(stderr,*)
     +   'replaced by '
         write(stderr,*)
     +   '   C1  -->  C1 + f(i) '
         write(stderr,*)
     +   '   P2'' -->  P2 + f(i) '
         write(stderr,*)
     +   'where f(i) = empirically-determined long-term average values'
         write(stderr,*)
     +   '<P1 - C1>i for GPS satellite PRNi. '
         write(stderr,*) ' '
         write(stderr,*)
     +   'In addition a few models of non-cross-correlator receivers  '
         write(stderr,*)
     +   '(e.g., Leica CRS1000) report C1 rather than P1.  For these  '
         write(stderr,*)
     +   'only C1 is replaced by  '
         write(stderr,*)
     +   '   C1  -->  C1 + f(i) '
         write(stderr,*) ' '
c
         call exit(0)
      endif
c
c     ... Open files
c
      iu = 19
      ou = 20
      bu = 21
      open(unit=iu, file=infile, status='old', iostat=ierr)
      if (ierr .ne. 0) then
         write(stderr,*) 'ERROR: Error opening input file.'
         write(stderr,*) 'iostat = ', ierr
         call exit(2)
      endif
      if (biasfile(1:1) .ne. ' ') then
        open(unit=bu, file=biasfile, status='old', iostat=ierr)
        if (ierr .ne. 0) then
           write(stderr,*) 'ERROR: Error opening bias file.'
           write(stderr,*) 'iostat = ', ierr
           call exit(2)
        endif
      endif
c
c     ... Read header records to get necessary file info
c
      line_n = 0
      call skip_header(iu, fmt_vers, rec_type, fixed, nobs, obs_type,
     +                 marker, line_n, first_obs, err) 
      if (err) then
         write(stderr,*) 'ERROR: Error reading RINEX header info.'
         call exit(2)
      endif
c
c     ... Find out if this receiver type needs changes to the observables
c
c     write(stderr,*) 'Testing if receiver type requires mods'
      mods = .false.
      c1only = .false.
c
      call upcase (rec_type, 20)
c
c     ... Read receiver types from external file, if input
c
      if (biasfile(1:1) .ne. ' ') then
        call read_rx_types(bu, stderr, c1p2list, nc1p2,
     +                     c1onlylist, nc1only, err)
        if (err) then
           call exit(3)
        endif
c
	if (nc1p2 .gt. 0) then
	  do i = 1, nc1p2
	    if (rec_type(1:20) .eq. c1p2list(i)) then
	      mods = .true.
	    end if
	  end do
	end if
c
	if (nc1only .gt. 0) then
	  do i = 1, nc1only
	    if (rec_type(1:20) .eq. c1onlylist(i)) then
	      mods = .true.
	      c1only = .true.
	    end if
	  end do
	end if
c
        go to 100
      endif
c
c     ... NOTE:  Add additional cross-correlation receiver types here.
c         These are the cross-corrlation type receivers which report
c         C1/P2' observables, both of which are modified.
      if (rec_type(1:10) .eq. 'ROGUE SNR-')     mods = .true.
      if (rec_type(1:17) .eq. 'AOA ICS-4000Z    ') mods = .true.
      if (rec_type(1:12) .eq. 'TRIMBLE 4000')   mods = .true.
      if (rec_type(1:13) .eq. 'TOPCON GP-DX1')  mods = .true.
c
c     ... NOTE:  
c         These are non-cross-corrlation receivers that report C1
c         instead of P1 (so do not require any P2 mod).
      if (rec_type(1:12) .eq. 'TRIMBLE 4700') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:12) .eq. 'TRIMBLE 5700') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:13) .eq. 'TRIMBLE MS750') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:12) .eq. 'LEICA RS500 ') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:13) .eq. 'LEICA CRS1000') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:12) .eq. 'LEICA SR9600') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:12) .eq. 'LEICA SR520 ') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:12) .eq. 'LEICA SR530 ') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:14) .eq. 'NOV MILLEN-RT2') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:14) .eq. 'NOV MILLEN-STD') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:15) .eq. 'NOV MILLEN-STDW') then
	 mods = .true.
	 c1only = .true.
      endif
      if (rec_type(1:18) .eq. 'NOV EURO4-1.00-222') then
	 mods = .true.
	 c1only = .true.
      endif
c
  100 continue
c
c     ... If user has forced corrections, set flags accordingly
      if (force(1:2) .ne. '  ') then
         if (force(1:4) .eq. 'C1P1') then
            mods = .true.
            c1only = .false.
            fixed = .false.
         endif
         if (force(1:4) .eq. 'C1  ') then
           mods = .true.
           c1only = .true.
           fixed = .false.
         endif
      endif
c
c     ... Record operational mode
      if (mods .and. .not.fixed .and. .not.c1only) then
         comment = 'CC C1, P2 converted to non-CC type       '
         if (force(1:2) .ne. '  ') then
            comment = 'CC C1, P2 forced to non-CC type          '
         endif
         write(stderr,*) comment
      else if (mods .and. .not.fixed .and. c1only) then
         comment = 'C1 converted to P1 for compatibility     '
         if (force(1:2) .ne. '  ') then
            comment = 'C1 forced to P1 for compatibility        '
         endif
         write(stderr,*) comment
      else if (mods .and. fixed) then
         comment = 'CC2nonCC previously run ... quitting     '
         write(stderr,*) comment
         mods = .false.
         call exit(0)
      else 
         comment = 'receiver type does not require mods ... quitting'
        write(stderr,*) comment
         mods = .false.
         call exit(0)
      endif
c
c    ... Identify specific observable fields needed
c
      c1f = 0
      p1f = 0
      p2f = 0
c
c     
      do i = 1, nobs
         if (obs_type(i) .eq. 'C1') c1f = i
         if (obs_type(i) .eq. 'P1') p1f = i
         if (obs_type(i) .eq. 'P2') p2f = i
      enddo
c
      if (c1f .eq. 0 .or.
     +    p2f .eq. 0) then
         write(stderr,*) 'ERROR: C1 or P2 field missing.'
         call exit(2)
      endif
c
c     ... Read bias values for this epoch from external file, if input
c
      if (biasfile(1:1) .ne. ' ') then
        call read_bias(bu, stderr, first_obs, bias, err)
        if (err) then
           call exit(2)
        endif
      endif
c
c     ... Save bias corrections into character strings to write into
c         into RINEX header records.
c         Bias values are nanoseconds, but write out in picoseconds to
c         save space by dropping the decimal points.
c
      call blnkstrng (cbias(1), 60)
      write(cbias(1),*)
     +  'average P1-C1 biases applied by CC2nonCC by PRN (ps):'
      call blnkstrng (cbias(2), 60)
      do i = 1, 10
	if (bias(i) .gt. -9.0d9)
     +  write (cbias(2)(6*(i-1)+1:6*(i-1)+6),'(x,i5)') 
     +    NINT((bias(i)*1.d3))
      end do
      call blnkstrng (cbias(3), 60)
      do i = 11, 20
	if (bias(i) .gt. -9.0d9)
     +  write (cbias(3)(6*(i-11)+1:6*(i-11)+6),'(x,i5)') 
     +    NINT((bias(i)*1.d3))
      end do
      call blnkstrng (cbias(4), 60)
      do i = 21, 30
	if (bias(i) .gt. -9.0d9)
     +  write (cbias(4)(6*(i-21)+1:6*(i-21)+6),'(x,i5)') 
     +    NINT((bias(i)*1.d3))
      end do
      call blnkstrng (cbias(5), 60)
      do i = 31, 40
	if (bias(i) .gt. -9.0d9)
     +  write (cbias(5)(6*(i-31)+1:6*(i-31)+6),'(x,i5)') 
     +    NINT((bias(i)*1.d3))
      end do
c
c     ... Now copy over the header info.
c
      rewind(unit=iu)
      open(unit=ou, file=outfile, status='unknown')
      line_n = 0

      call copy_header(iu, ou, version, log_line, comment, cbias,
     +                 fmt_vers, nobs, obs_type, line_n, err)
      if (err) then
         write(stderr,*) 'ERROR: Error copying header information.'
         call exit(2)
      endif
c
c     ... Now fix up the file, which must be rewound first.
c     ... Initialization for loop
c
      irec = 0
      nwarn = 0
      eof = .false.

      do while (.not.eof .and. .not.err)
c      
         call read_rec(iu, nobs, itime, sec, msec, flag, numsat, prn,
     +            char, clockerr, obs, lli, snr, eventrecs, err, eof)
c
         if (.not.err .and. flag .le. 1) then
            irec = irec + 1
c
            do j = 1, numsat
c
c              ... Transform C1  --> P1  by adding <P1-C1> bias
c                  Transform P2' --> P2  by adding <P1-C1> bias
c              (Note: units are meters for pseudorange in RINEX files
c                     but P1-C1 bias values are nanoseconds starting 
c                     with version 2.0 of this program)
c
c Nacho08 - apply corrections only to GPS satellites not to GLONASS ones
	       if ((bias(prn(j)).gt.-9.0d9).and.
     +	              ((char(j).eq.'G').or.(char(j).eq.' '))
     + 							      ) then
                 if (obs(c1f,j) .ne. 0.0d0)
     +             obs(c1f,j) = obs(c1f,j) + (bias(prn(j))/3.335641d0)
                 if (obs(p2f,j) .ne. 0.0d0 .and. .not.c1only)
     +             obs(p2f,j) = obs(p2f,j) + (bias(prn(j))/3.335641d0)
               else
		 if (nwarn .eq. 0) then
		   nwarn = 1
		   charwarn(1) = char(j)
		   prnwarn(1) = prn(j)
                 else
		   i = 1
		   do while (prnwarn(i) .ne. prn(j) .and.
     +                       i .le. nwarn)
		     i = i + 1
		     if (i .gt. nwarn) then
		       charwarn(i) = char(j)
		       prnwarn(i) = prn(j)
		       nwarn = i
		     endif
		   enddo
		 end if
	       endif
            enddo
c
            if (.not.eof)
     +      call write_rec(ou, fmt_vers, nobs, itime, sec, msec, flag,
     +                     numsat, prn, char, clockerr, obs, lli, snr,
     +                     eventrecs)
         endif
      enddo
c
      write(stderr,*) irec, ' records processed.'
      if (nwarn .gt. 0) then
        write(stderr,'(A,40(1X,A1,I2.2))') 
     +	   'WARNING: No P1-C1 bias for satellites ',
     +       (charwarn(i),prnwarn(i), i = 1, nwarn)
      end if
c
      close(ou)
      write(stderr,*) '       '
c
      end
c
c
