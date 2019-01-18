* 2018-11-15  Updated the libname using the SQLSVR engine instead ;

*--- Initialize page with and height (43) for output etc;
options ls=130 ps=43 nodate nonumber nocenter source source2 nostimer msglevel=N
        notes dsnferr serror fmterr details
        mautosource nomstored mrecall
        nomacrogen nosymbolgen nomtrace nomlogic nomprint
        mergenoby=WARN validvarname=upcase
        formchar='|----|+|---+=|-//<>*'
;

*--- Route the output to one file and the log to another file;
*proc printto new log='z:\slask\jconst10.log' print='z:\slask\jconst10.lst'; run;

*-- Allocate the database;
libname socmob oledb provider=sqloledb.1 properties=('Data Source'="MEB-SQL02" 'Integrated Security'=SSPI 'Initial Catalog'=MGR_socmob) schema=MGR_socmob;

*libname SOCMOB oledb provider=sqloledb.1 properties=('Data Source'="MEB-SQL02" 'Integrated Security'=SSPI 'Initial Catalog'=CERVIX_SOCMOB) schema= SOCMOB;

*libname SOCMOB2 oledb provider=sqloledb.1 properties=('Data Source'="MEB-SQL02" 'Integrated Security'=SSPI 'Initial Catalog'= CERVIX_SOCMOB2) schema=CERVIX_SOCMOB2;

*-- Create output formats. These lables will be PRINTED instead of the codes;
proc format;
  value sibtyp 1='Helsyskon'
               2='HalvHelsyskonMor'
               3='HalvHelsyskonFar'
               4='HalvsyskonMor'
               5='HalvsyskonFar'
               0='No sibling';
run;


*-- Select all children born after 1991;
proc sql;
  create table s1 as
  select lopnr as child, put(kon,1.) as child_sex length=1,
         input(compress(fodarmanad)||'15', yymmdd10.) as birth_dat length=4 format=yymmdd8. label='Date of Birth'
  from socmob.individ_clean
  where fodelselandgrupp eq 'Sverige' AND
        substr(left(fodarmanad),1,4) in ('1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002')
  ;
  *select year(birth_dat) as year, count(*) from s1 group by year;
quit;


*-- Join in mother and father id;
proc sql;
  create table s2 as
  select a.*, b.lopnrmor as mother, b.lopnrfar as father
  from s1 as a
  left join socmob.bioadforaldrar_clean as b
    on a.child = b.lopnr
  ;
quit;

*-- Subset to families where both mother and father is known;
proc sql;
  create table s3 as select * from s2 where mother > .z and father > .z;
quit;

*-- Prinout to check numbers are reasonable;
proc sql;
create table chk1 as select child, count(*) as n from t0 group by child;
quit;

*-- Mothers siblings and birth info;
proc sql;
  create table s4(label='Mothers siblings') as
  select a.child, a.birth_dat, a.mother, a.father, b.lopnradsyskon as masib1 label='Mothers sibling',
         d.lopnrhelsyskon as masib_full label='Mothers full sibling',
         e.lopnrhalvsyskon as masib_half label='Mothers half sibling',
         input(compress(c.fodarmanad)||'15', yymmdd10.) as mob_dat length=4 format=yymmdd8. label='Mothers birth date'
  from s3 as a
  left join socmob.adsyskon_clean as b
    on a.mother = b.lopnr
  left join socmob.individ_clean as c
    on a.mother = c.lopnr
  left join socmob.helsyskon_clean as d
    on a.mother = d.lopnr
  left join socmob.halvsyskon_clean as e
    on a.mother = e.lopnr
  ;
quit;

data s5;
  drop masib1 masib_full masib_half;
  format child mother father ;
  informat child mother father ;
  attrib masib_typ length=$4 label='Sibling type' masib length=8 label='Maternal sibling ID';

  set s4;
  if masib_full LE .z and masib_half LE .z then do;
    masib_typ='NA'; masib=.n; output;
  end;
  else do;
    if masib_full GT .z then do;
      masib_typ='Full'; masib=masib_full; output;
    end;
    if masib_half GT .z then do;
      masib_typ='Half'; masib=masib_half;output;
    end;
  end;
run;

*--------------------------------------------------------------------------------;
* Add information about child's sex and parental siblings sex                    ;
*--------------------------------------------------------------------------------;
proc sql;
  create table s6 as
  select a.*, put(kon,1.) as child_sex length=1
  from s5 as a
  left join socmob.individ_clean as b
    on a.child=b.lopnr
  ;
quit;

proc sql;
  create table s7 as
  select a.*, put(kon,1.) as masib_sex length=1,
         input(compress(b.fodarmanad)||'15', yymmdd10.) as sib_birth_dat length=4 format=yymmdd8. label='Sib Date of Birth'
  from s6 as a
  left join socmob.individ_clean as b
    on a.masib=b.lopnr
  ;
quit;
*-------------------------------------------------------------------;
* Add censor time: date of death  and  date of emigration           ;
*-------------------------------------------------------------------;
*Date of death;
proc sql;
create table a as
select lopnr as id, doddatum as deathdate 
from socmob.individ_clean
;
quit;

proc sql;
create table b as
select lopnr as id, dodsdat as deathdate
from socmob.death2011
;
quit;

data death0;
set a b;
run;
 
data death slask1 slask2; 
  drop deathdate;
  attrib death_dat length=4 format=yymmdd10. label='Date of Death';
  set death0(keep=id deathdate);
  if length(trim(left(deathdate))) GE 8 then do;
	if substr(left(reverse(deathdate)),1,4) in ('9220','1320') then deathdate=substr(deathdate,1,4)||'0227';
	else if substr(left(reverse(deathdate)),1,4)='0000' then deathdate=substr(deathdate,1,4)||'0715';
    else if substr(left(reverse(deathdate)),1,2)='00' then deathdate=substr(deathdate,1,6)||'15';
    death_dat=input(deathdate, yymmdd10.);
	if death_dat le .z then output slask1;
	else output death;
  end;
  else output slask2;
run;

*Date of emigration;
proc sql;
create table emigrat0 as
select lopnr as id, sistautvar as emidate
from socmob.individ_clean;
quit;

data emigrat; 
drop emidate;
attrib emi_dat length=4 format=yymmdd10. label='Date of Emigration';
  set emigrat0(keep=id emidate);
  if length(trim(left(emidate))) GE 4 then emi_dat=input(substr(emidate,1,4)||'0715', yymmdd10.);
else delete;
run;

*-------------------------------------------------------------;
* Add information about offspring emigration and death        ;
*-------------------------------------------------------------;
proc sort data=emigrat;by id;run;
proc sort data=s7;by child;run;
data s8;
  drop _c_;
  retain _c_ 0;
  merge s7(in=s7) emigrat(in=emigrat rename=(id=child)) end=eof;by child;
  if emigrat and not s7 then delete;
  else if emigrat and s7 then _c_=_c_+1;
  if eof then put 'Note: There are ' _c_ ' children censored due to emigration';
run;

proc sort data=death;by id;run;
proc sort data=s8;by child;run;
data s9;
  drop _c_;
  retain _c_ 0;
  merge s8(in=s8) death(in=death rename=(id=child)) end=eof;by child;
  if death and not s8 then do;
    if eof then put 'Note: There are ' _c_ ' children censored due to death';
    delete;
  end;
  else if death and s8 then _c_=_c_+1;
  if eof then put 'Note: There are ' _c_ ' children censored due to death';
run;

*-------------------------------------------------------------;
* Keep those children lived up to 2 years old                 ;
*-------------------------------------------------------------;
data s10;
  drop _D_ _E_;
  retain _d_ _e_ 0;
  set s9 end=eof;
  if .z<(death_dat-birth_dat)/365.25<2 then do; delete; _d_=_d_+1; end;
  else if .z<(emi_dat-birth_dat)/365.25<2 then do; delete; _e_=_e_+1; end;
  if eof then put 'Note: There are' _d_  _e_'  children deleted due to death or emigration before 2';
run; 

***************************************************************; 
*---------------------------------------------;
* Derive ASD and all psychiatric diagnoses    ;
*---------------------------------------------;

*- 1st a view to read the data from the table;
proc datasets lib=work mt=all nolist;delete p1 p1_in p1_out;quit;
data p1_in / view=p1_in;
  drop indatum_date slask;
  retain psource 'i';
  length hdia bdia1-bdia3 $9 slask $10;
  format hdia bdia1-bdia3 age lopnr;
  attrib diag_dat length=4 format=yymmdd10. label='Date of Diagnosis' age length=4 label='Age';

  set study.inpatient6410_clean(keep=lopnr alder indatum_date hdia bdia1-bdia3 where=(hdia ne '') rename=(alder=age));

  if "%upcase(&sysscpl)" NE "LINUX" then do;
    if length(trim(left(indatum_date))) GE 8 then diag_dat=input(indatum_date, yymmdd10.);
    else delete;
  end;
  else do;
    slask=put(datepart(indatum_date),yymmdd10.);
    if length(trim(left(slask))) GE 8 then diag_dat=input(slask, yymmdd10.);
    else delete;
  end;
run;
data p1_out / view=p1_out;
  drop indatum slask;
  retain psource 'o';
  length hdia bdia1-bdia3 $9;
  format hdia bdia1-bdia3 age lopnr;
  attrib diag_dat length=4 format=yymmdd10. label='Date of Diagnosis' age length=4 label='Age';

  set study.outpatient0610_clean(keep=lopnr alder indatum hdia bdia1-bdia3 where=(hdia ne '') rename=(alder=age));

  if "%upcase(&sysscpl)" NE "LINUX" then do;
    if length(trim(left(indatum))) GE 8 then diag_dat=input(indatum, yymmdd10.);
    else delete;
  end;
  else do;
    slask=put(datepart(indatum),yymmdd10.);
    if length(trim(left(slask))) GE 8 then diag_dat=input(slask, yymmdd10.);
    else delete;
  end;

run;

*- Next, transpose the data;
data p2;
  drop hdia bdia1-bdia3;
  length diag $9;
  retain diag '';
  array bi $9 hdia bdia1-bdia3;
  set p1_in p1_out;
  do over bi;
    if bi ne '' then do;
      diag=compress(bi);output;
    end;
  end;
run;
proc sort data=p2 out=p3 nodupkey;by lopnr diag diag_dat;run;
proc sort data=p3 out=p4 nodupkey;by lopnr diag;run;
data p5;
  length icd 3;
  set p4;
  if diag_dat GE '01JAN1997'd then icd=10;
  else if diag_dat GE '01JAN1987'd then icd=9;
  else if diag_dat GE '01JAN1969'd then icd=8;
  else icd=7;
run;

*-- Derive Psychiatry History;
data h0;
  length any_psych 3;
  retain any_psych 1;
  set p5(keep=lopnr icd diag diag_dat);

  *-- Any mental illness;
  if (icd eq 10 and substr(diag,1,1) in ('F')) or 
	 (icd lt 10 and substr(diag,1,3) in ('295','296','297','298','299','300','301','303','304',
                                         '306','308','310','311','312','313','314','315'));
  
run;

*-- Now derive specific diagnostic groups, e.g. ASD;
data h1;
  drop diag2 first_char;
  length asd 3 condition $3 first_char $1;
  set h0(keep=lopnr icd diag diag_dat any_psych);
/*  asd=0; id=0; sid=0; depress=0; anxiety=0; subuse=0; bipolar=0; compuls=0; ADHD=0; affective=0; sc=0; spd=0;*/
  asd = 0;
  condition = 'OTH';
  diag = tranwrd(diag, ',', '.');
  diag2 = compress(diag,'ABWXEP');
  first_char = substr(diag,1,1);
  
  *-- Autism as a separate variable;
  if diag_dat GT '01JAN1987'd then do;
    if diag in: ('F840') then asd=1;
    if diag in: ('F845') then asd=2;
    if diag in: ('F841','F848','F849') then asd=3;
    if icd=9 and diag in: ('299A') then asd=1; ** Childhood autism **;
  end;
  
  *-- ASD and subtypes;
  if diag_dat GT '01JAN1987'd then do;
    if diag in: ('F840')               or
       diag in: ('F845')               or
       diag in: ('F841','F848','F849') or
       icd=9 and diag in: ('299A')     then do; ** Childhood autism **;
	   condition='ASD'; output;
	end;
  end;
  *-- AD;
  if diag_dat GT '01JAN1987'd then do;
    if diag in: ('F840')             or
     icd=9 and diag in: ('299A')   then do;
     condition='AD'; output;
    end;
  end;
   *-- Asperger;
  if diag_dat GT '01JAN1987'd then do;
    if diag in: ('F845')  then do;
     condition='AS'; output;
    end;
  end; 
   *-- PDD-NOS;
  if diag_dat GT '01JAN1987'd then do;
    if diag in: ('F841','F848','F849')  then do;
     condition='PDD'; output;
    end;
  end; 

  *-- Intellectual disability;
  if icd eq 10 and first_char in ('F') and 70=<substr(diag2,2,2)<80         or
	 icd eq 9 and substr(diag,1,3) in ('317','318','319')                   or
	 icd eq 8 and substr(diag,1,3) in ('310','311','312','313','314','315') then do;
	 condition='ID'; output;
  end;
 
  *-- Severe intellectual disability;
  if icd eq 10 and diag in: ('F72','F73')           or
	 icd eq 9 and diag in ('318.1','318.2')         or
	 icd eq 8 and substr(diag,1,3) in ('313','314') then do;
     condition='SID'; output;
  end;
 
  *-- Depression;
  if icd eq 10 and diag in:('F32','F33','F34.1','F34.8','F34.9','F43.21')            or
     icd eq 9 and diag in: ('296.2','296.3','300.4','301.12','309.1','311','296.82') or
     icd eq 8 and substr(diag,1,5) in ('298.0','300.4')                              then do;
	 condition='DEP'; output;
  end;

  *-- Anxiety disorder;
  if icd eq 10 and diag in:('F40','F41')     or
     icd eq 9 and diag in: ('300.0','300.2') or
     icd eq 8 and diag in: ('300.0','300.2') then do;
	 condition='ANX'; output;
  end;
  
  *--Substance use disorder;
  if icd eq 10 and first_char in ('F') and 10=<substr(diag2,2,2)<20 or
	 icd eq 9 and diag in: ('303','304','305')                      or
	 icd eq 8 and diag in: ('303','304')                            then do;
	 condition='SUB'; output;
  end;

  *--Bipolar disorder;
  if icd eq 10 and diag in:('F30','F31','F34.0')                       or
	 icd eq 9 and diag in: ('296.0','296.1','296.4','296.5','296.6',
                            '296.7','296.80','296.81','296.89','298B') or
	 icd eq 8 and diag in: ('296','298.1')                             then do;
	 condition='BIP';  output;
  end;

  *--Compulsive disorder;
  if icd eq 10 and diag in:('F42')  or
	 icd eq 9 and diag in ('300.3') or
	 icd eq 8 and diag in ('300.3') then do;
	 condition='COM'; output;
  end;

  *--ADHD;
  if icd eq 10 and diag in:('F90') or
	 icd eq 9 and diag in:('314')  then do;
	 condition='ADH'; output;
  end;

  *--Affective disorder;
  if icd eq 10 and diag in:('F38','F39') then do;
     condition='AFF';  output;
  end;

  *--Schizophrenia;
  if icd eq 10 and diag in:('F20','F22','F23','F24','F25','F28','F29') or
     icd eq 9 and diag in: ('295','297','298')                         or
     icd eq 9 and diag in ('298C','298E','298W','298X')                or
     icd eq 8 and diag in: ('295','297','299','298.2','298.3','298.9') then do;
	 condition='SCH'; output;
  end;  

  *--Schizoid personality disorder;
  if icd eq 10 and diag in ('F60.1') or
     icd eq 9 and diag in: ('301.2') then do;
	 condition='SPD'; output;
  end;
 
run;

* proc freq data=h1; *table condition; *run;

*-Keep first diagnosis date of each condition;
proc sort data=h1; by lopnr condition diag_dat;run;
data h2;
  set h1;
  by lopnr condition diag_dat;
  if first.condition;
run;

*-Keep first diagnosis date of 'any mental illness';
proc sort data=h0; by lopnr diag_dat; run;
data h3;
  set h0;
  by lopnr diag_dat;
  if first.lopnr;
run;


*--------------------------------------------------------------------------------;
* Add information about family psychiatric history                               ;
*--------------------------------------------------------------------------------;

*-Outcome among children;
data ana0;
 length outcome $3;
 set s10;
 outcome='ASD';output;
 outcome='AD';output;
run;

proc sort data=ana0; by outcome child; run;
proc sort data=h2; by condition lopnr; run;
data ana1;
 drop icd diag any_psych;
 length event 3;
 merge ana0 (in=ana0) h2 (in=h2 rename=(lopnr=child condition=outcome diag_dat=child_diagdat) where=(outcome in ('ASD','AD')));
 by outcome child;
 if h2 then event=1; else event=0;
 if ana0;
run;

*-Exposure: ASD conditions among mother's siblings + Covariate: psychiatric conditions among mother's siblings;
proc sort data=ana1; by masib; run;
proc sort data=h2; by lopnr; run;

*- ASD among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_ASD 3;
 merge ana1 (in=ana1) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'ASD'));
 by masib;
 if h2 then exp_ASD=1; 
 * if h2 and diag_dat lt birth_dat then exp_ASD=1; **Select those diagnosed before child's birth date**;
 else exp_ASD=0;
 if ana1;
run;

*- AD among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_AD 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'AD'));
 by masib;
 if h2 then exp_AD=1; 
 * if h2 and diag_dat lt birth_dat then exp_AD=1; **Select those diagnosed before child's birth date**;
 else exp_AD=0;
 if ana2;
run;

*- Asperger among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_AS 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'AS'));
 by masib;
 if h2 then exp_AS=1; 
 * if h2 and diag_dat lt birth_dat then exp_AS=1; **Select those diagnosed before child's birth date**;
 else exp_AS=0;
 if ana2;
run; 

*- PDD-NOS among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_PDD 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'PDD'));
 by masib;
 if h2 then exp_PDD=1; 
 * if h2 and diag_dat lt birth_dat then exp_PDD=1; **Select those diagnosed before child's birth date**;
 else exp_PDD=0;
 if ana2;
run; 

*- Intellectual disability among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_ID 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'ID'));
 by masib;
 if h2 then exp_ID=1; 
 * if h2 and diag_dat lt birth_dat then exp_ID=1; **Select those diagnosed before child's birth date**;
 else exp_ID=0;
 if ana2;
run; 

*- Severe intellectual disability among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_SID 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'SID'));
 by masib;
 if h2 then exp_SID=1; 
 * if h2 and diag_dat lt birth_dat then exp_SID=1; **Select those diagnosed before child's birth date**;
 else exp_SID=0;
 if ana2;
run; 

*- Depression among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_DEP 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'DEP'));
 by masib;
 if h2 then exp_DEP=1; 
 * if h2 and diag_dat lt birth_dat then exp_DEP=1; **Select those diagnosed before child's birth date**;
 else exp_DEP=0;
 if ana2;
run; 

*- Anxiety disorder among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_ANX 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'ANX'));
 by masib;
 if h2 then exp_ANX=1; 
 * if h2 and diag_dat lt birth_dat then exp_ANX=1; **Select those diagnosed before child's birth date**;
 else exp_ANX=0;
 if ana2;
run; 

*- Substance use disorder among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_SUB 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'SUB'));
 by masib;
 if h2 then exp_SUB=1; 
 * if h2 and diag_dat lt birth_dat then exp_SUB=1; **Select those diagnosed before child's birth date**;
 else exp_SUB=0;
 if ana2;
run; 

*- Bipolar disorder among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_BIP 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'BIP'));
 by masib;
 if h2 then exp_BIP=1; 
 * if h2 and diag_dat lt birth_dat then exp_BIP=1; **Select those diagnosed before child's birth date**;
 else exp_BIP=0;
 if ana2;
run; 

*- Compulsive disorder among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_COM 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'COM'));
 by masib;
 if h2 then exp_COM=1; 
 * if h2 and diag_dat lt birth_dat then exp_COM=1; **Select those diagnosed before child's birth date**;
 else exp_COM=0;
 if ana2;
run; 

*- ADHD among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_ADHD 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'ADH'));
 by masib;
 if h2 then exp_ADHD=1; 
 * if h2 and diag_dat lt birth_dat then exp_ADHD=1; **Select those diagnosed before child's birth date**;
 else exp_ADHD=0;
 if ana2;
run; 

*- Affective disorder among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_AFF 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'AFF'));
 by masib;
 if h2 then exp_AFF=1; 
 * if h2 and diag_dat lt birth_dat then exp_AFF=1; **Select those diagnosed before child's birth date**;
 else exp_AFF=0;
 if ana2;
run; 

*- Schizophrenia among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_SCH 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'SCH'));
 by masib;
 if h2 then exp_SCH=1; 
 * if h2 and diag_dat lt birth_dat then exp_SCH=1; **Select those diagnosed before child's birth date**;
 else exp_SCH=0;
 if ana2;
run; 

*- Schizoid personality disorder among mother's siblings;
data ana2;
 drop icd diag any_psych condition diag_dat;
 length exp_SPD 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=masib) where=(condition = 'SPD'));
 by masib;
 if h2 then exp_SPD=1; 
 * if h2 and diag_dat lt birth_dat then exp_SPD=1; **Select those diagnosed before child's birth date**;
 else exp_SPD=0;
 if ana2;
run; 

*- Any mental illness among mother's siblings;
proc sort data=h3; by lopnr; run;
data ana2;
 drop icd diag any_psych diag_dat;
 length exp_PSYCH 3;
 merge ana2 (in=ana2) h3 (in=h3 rename=(lopnr=masib));
 by masib;
 if h3 then exp_PSYCH=1; 
 * if h3 and diag_dat lt birth_dat then exp_PSYCH=1; **Select those diagnosed before child's birth date**;
 else exp_PSYCH=0;
 if ana2;
run; 


*-Exposure: ASD conditions among mothers + Covariate: psychiatric conditions among mothers;
proc sort data=ana2; by mother; run;

*- ASD among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_ASD 3;
 merge ana2 (in=ana2) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'ASD'));
 by mother;
 if h2 then mom_ASD=1; 
 * if h2 and diag_dat lt birth_dat then mom_ASD=1; **Select those diagnosed before child's birth date**;
 else mom_ASD=0;
 if ana2;
run;

*- AD among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_AD 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'AD'));
 by mother;
 if h2 then mom_AD=1; 
 * if h2 and diag_dat lt birth_dat then mom_AD=1; **Select those diagnosed before child's birth date**;
 else mom_AD=0;
 if ana3;
run;

*- Asperger among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_AS 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'AS'));
 by mother;
 if h2 then mom_AS=1; 
 * if h2 and diag_dat lt birth_dat then mom_AS=1; **Select those diagnosed before child's birth date**;
 else mom_AS=0;
 if ana3;
run; 

*- PDD-NOS among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_PDD 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'PDD'));
 by mother;
 if h2 then mom_PDD=1; 
 * if h2 and diag_dat lt birth_dat then mom_PDD=1; **Select those diagnosed before child's birth date**;
 else mom_PDD=0;
 if ana3;
run; 

*- Intellectual disability among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_ID 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'ID'));
 by mother;
 if h2 then mom_ID=1; 
 * if h2 and diag_dat lt birth_dat then mom_ID=1; **Select those diagnosed before child's birth date**;
 else mom_ID=0;
 if ana3;
run; 

*- Severe intellectual disability among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_SID 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'SID'));
 by mother;
 if h2 then mom_SID=1; 
 * if h2 and diag_dat lt birth_dat then mom_SID=1; **Select those diagnosed before child's birth date**;
 else mom_SID=0;
 if ana3;
run; 

*- Depression among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_DEP 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'DEP'));
 by mother;
 if h2 then mom_DEP=1; 
 * if h2 and diag_dat lt birth_dat then mom_DEP=1; **Select those diagnosed before child's birth date**;
 else mom_DEP=0;
 if ana3;
run; 

*- Anxiety disorder among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_ANX 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'ANX'));
 by mother;
 if h2 then mom_ANX=1; 
 * if h2 and diag_dat lt birth_dat then mom_ANX=1; **Select those diagnosed before child's birth date**;
 else mom_ANX=0;
 if ana3;
run; 

*- Substance use disorder among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_SUB 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'SUB'));
 by mother;
 if h2 then mom_SUB=1; 
 * if h2 and diag_dat lt birth_dat then mom_SUB=1; **Select those diagnosed before child's birth date**;
 else mom_SUB=0;
 if ana3;
run; 

*- Bipolar disorder among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_BIP 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'BIP'));
 by mother;
 if h2 then mom_BIP=1; 
 * if h2 and diag_dat lt birth_dat then mom_BIP=1; **Select those diagnosed before child's birth date**;
 else mom_BIP=0;
 if ana3;
run; 

*- Compulsive disorder among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_COM 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'COM'));
 by mother;
 if h2 then mom_COM=1; 
 * if h2 and diag_dat lt birth_dat then mom_COM=1; **Select those diagnosed before child's birth date**;
 else mom_COM=0;
 if ana3;
run; 

*- ADHD among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_ADHD 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'ADH'));
 by mother;
 if h2 then mom_ADHD=1; 
 * if h2 and diag_dat lt birth_dat then mom_ADHD=1; **Select those diagnosed before child's birth date**;
 else mom_ADHD=0;
 if ana3;
run; 

*- Affective disorder among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_AFF 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'AFF'));
 by mother;
 if h2 then mom_AFF=1; 
 * if h2 and diag_dat lt birth_dat then mom_AFF=1; **Select those diagnosed before child's birth date**;
 else mom_AFF=0;
 if ana3;
run; 

*- Schizophrenia among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_SCH 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'SCH'));
 by mother;
 if h2 then mom_SCH=1; 
 * if h2 and diag_dat lt birth_dat then mom_SCH=1; **Select those diagnosed before child's birth date**;
 else mom_SCH=0;
 if ana3;
run; 

*- Schizoid personality disorder among mothers;
data ana3;
 drop icd diag any_psych condition diag_dat;
 length mom_SPD 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=mother) where=(condition = 'SPD'));
 by mother;
 if h2 then mom_SPD=1; 
 * if h2 and diag_dat lt birth_dat then mom_SPD=1; **Select those diagnosed before child's birth date**;
 else mom_SPD=0;
 if ana3;
run; 

*- Any mental illness among mothers;
proc sort data=h3; by lopnr; run;
data ana3;
 drop icd diag any_psych diag_dat;
 length mom_PSYCH 3;
 merge ana3 (in=ana3) h3 (in=h3 rename=(lopnr=mother));
 by mother;
 if h3 then mom_PSYCH=1; 
 * if h3 and diag_dat lt birth_dat then mom_PSYCH=1; **Select those diagnosed before child's birth date**;
 else mom_PSYCH=0;
 if ana3;
run; 


*-Exposure: ASD conditions among fathers + Covariate: psychiatric conditions among fathers;
proc sort data=ana3; by father; run;

*- ASD among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_ASD 3;
 merge ana3 (in=ana3) h2 (in=h2 rename=(lopnr=father) where=(condition = 'ASD'));
 by father;
 if h2 then dad_ASD=1; 
 * if h2 and diag_dat lt birth_dat then dad_ASD=1; **Select those diagnosed before child's birth date**;
 else dad_ASD=0;
 if ana3;
run;

*- AD among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_AD 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'AD'));
 by father;
 if h2 then dad_AD=1; 
 * if h2 and diag_dat lt birth_dat then dad_AD=1; **Select those diagnosed before child's birth date**;
 else dad_AD=0;
 if ana4;
run;

*- Asperger among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_AS 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'AS'));
 by father;
 if h2 then dad_AS=1; 
 * if h2 and diag_dat lt birth_dat then dad_AS=1; **Select those diagnosed before child's birth date**;
 else dad_AS=0;
 if ana4;
run; 

*- PDD-NOS among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_PDD 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'PDD'));
 by father;
 if h2 then dad_PDD=1; 
 * if h2 and diag_dat lt birth_dat then dad_PDD=1; **Select those diagnosed before child's birth date**;
 else dad_PDD=0;
 if ana4;
run; 

*- Intellectual disability among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_ID 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'ID'));
 by father;
 if h2 then dad_ID=1; 
 * if h2 and diag_dat lt birth_dat then dad_ID=1; **Select those diagnosed before child's birth date**;
 else dad_ID=0;
 if ana4;
run; 

*- Severe intellectual disability among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_SID 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'SID'));
 by father;
 if h2 then dad_SID=1; 
 * if h2 and diag_dat lt birth_dat then dad_SID=1; **Select those diagnosed before child's birth date**;
 else dad_SID=0;
 if ana4;
run; 

*- Depression among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_DEP 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'DEP'));
 by father;
 if h2 then dad_DEP=1; 
 * if h2 and diag_dat lt birth_dat then dad_DEP=1; **Select those diagnosed before child's birth date**;
 else dad_DEP=0;
 if ana4;
run; 

*- Anxiety disorder among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_ANX 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'ANX'));
 by father;
 if h2 then dad_ANX=1; 
 * if h2 and diag_dat lt birth_dat then dad_ANX=1; **Select those diagnosed before child's birth date**;
 else dad_ANX=0;
 if ana4;
run; 

*- Substance use disorder among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_SUB 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'SUB'));
 by father;
 if h2 then dad_SUB=1; 
 * if h2 and diag_dat lt birth_dat then dad_SUB=1; **Select those diagnosed before child's birth date**;
 else dad_SUB=0;
 if ana4;
run; 

*- Bipolar disorder among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_BIP 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'BIP'));
 by father;
 if h2 then dad_BIP=1; 
 * if h2 and diag_dat lt birth_dat then dad_BIP=1; **Select those diagnosed before child's birth date**;
 else dad_BIP=0;
 if ana4;
run; 

*- Compulsive disorder among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_COM 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'COM'));
 by father;
 if h2 then dad_COM=1; 
 * if h2 and diag_dat lt birth_dat then dad_COM=1; **Select those diagnosed before child's birth date**;
 else dad_COM=0;
 if ana4;
run; 

*- ADHD among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_ADHD 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'ADH'));
 by father;
 if h2 then dad_ADHD=1; 
 * if h2 and diag_dat lt birth_dat then dad_ADHD=1; **Select those diagnosed before child's birth date**;
 else dad_ADHD=0;
 if ana4;
run; 

*- Affective disorder among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_AFF 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'AFF'));
 by father;
 if h2 then dad_AFF=1; 
 * if h2 and diag_dat lt birth_dat then dad_AFF=1; **Select those diagnosed before child's birth date**;
 else dad_AFF=0;
 if ana4;
run; 

*- Schizophrenia among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_SCH 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'SCH'));
 by father;
 if h2 then dad_SCH=1; 
 * if h2 and diag_dat lt birth_dat then dad_SCH=1; **Select those diagnosed before child's birth date**;
 else dad_SCH=0;
 if ana4;
run; 

*- Schizoid personality disorder among fathers;
data ana4;
 drop icd diag any_psych condition diag_dat;
 length dad_SPD 3;
 merge ana4 (in=ana4) h2 (in=h2 rename=(lopnr=father) where=(condition = 'SPD'));
 by father;
 if h2 then dad_SPD=1; 
 * if h2 and diag_dat lt birth_dat then dad_SPD=1; **Select those diagnosed before child's birth date**;
 else dad_SPD=0;
 if ana4;
run; 

*- Any mental illness among fathers;
proc sort data=h3; by lopnr; run;
data ana4;
 drop icd diag any_psych;
 length dad_PSYCH 3;
 merge ana4 (in=ana4) h3 (in=h3 rename=(lopnr=father));
 by father;
 if h3 then dad_PSYCH=1; 
 * if h3 and diag_dat lt birth_dat then dad_PSYCH=1; **Select those diagnosed before child's birth date**;
 else dad_PSYCH=0;
 if ana4;
run; 

*--------------------------------------------------------------------------------;
* Add information about end of follow-up and create tte                          ;
*--------------------------------------------------------------------------------;

proc sql; select max(diag_dat) as max format=date9. from ana4;run; *Latest date of diagnosis: 31dec2010;

proc sort data=ana4; by outcome child; run;
data ana5;
  retain endfu_dat '31DEC2010'd;
  set ana4;
  min = min(death_dat, emi_dat, diag_dat, endfu_dat);
  if min=diag_dat then do;
    cens_type='A'; exit_dat=min;
  end;
  else if min=death_dat then do;
    cens_type='D'; exit_dat=min;
  end;
  else if min=emi_dat then do;
    cens_type='E'; exit_dat=min;
  end;
  else if min=endfu_dat then do;
    cens_type='F'; exit_dat=min;
  end;
  else abort;
  tte=exit_dat-birth_dat;
run;

*After data cleaning and management, ana5 is the complete dataset for all analyses;
