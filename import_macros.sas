/*Macros and tools, used for the creation of the SAS Code from the CRF page*/

/*A simple numeric function to check if strings are equal,
or one is a part of another IN TERMS OF WORDS. 
Example: 'Cat' will be part of 'Lazy cat'
but WILL NOT be a part of 'Lazy cats'*/
%macro search_text_addfunc(outlib = );
	proc fcmp outlib = &outlib.;
		*A simple function to check if: ;
		*string 1 = string 2 then return 1; 
		*string 1 is a part of string 2 then return 12;
		*string 2 is a part of string 1 then return 21;
		*other cases - return 0;
 
		function compare_strings(str1 $, str2 $);
		
			length str11 str22 $200;
			*Check if any of strings is empty. Do not compare then;
			if missing(str1) then return(-1);
			if missing(str2) then return(-2);
			if missing(str1) and missing(str2) then return(-99);

			*Replace blanks for comparison by words. Remove punctuation characters;
			str11 = "_"!!tranwrd(strip(compress(str1,"+/|\:;-!@#_$%^&?*()[]")), " ", "_")!!"_";
			str22 = "_"!!tranwrd(strip(compress(str2,"+/|\:;-!@#_$%^&?*()[]")), " ", "_")!!"_";

			if upcase(strip(str11)) eq upcase(strip(str22)) then return(1);
			else if find(str11, str22, 'ti') gt 0 then return(21);
			else if find(str22, str11, 'ti') gt 0 then return(12);
			else return(0);
		endsub;
	run;
%mend search_text_addfunc;


/****************          1 - CHECK_DS          ****************/
/*A simple macro to check if a dataset exist,
and how many observations does it contain.
Macro variable indicates following options:
1 - dataset is not empty
0 - dataset is empty
-1 - dataset does not exist*/

%macro checkds
			  (dsin     = ,/*input dataset name to check existence*/
			   flagname =  /*macro variable name flag, indicating the result*/);
   %global &flagname.;
   %if %sysfunc(exist(&dsin.)) %then %do;
      %put NO%str(TE): Dataset &dsin. found.;

	  %let dsid = %sysfunc(open(&dsin.));
	  %let nobs = %sysfunc(attrn(&dsid., nlobs));
	  %let dsid = %sysfunc(close(&dsid.));

	  %if &nobs. eq 0 %then %do;
		  %put NO%str(TE): Dataset &dsin. contains 0 observations!;
		  %let &flagname. = 0;
	  %end;
	  %else %do;
		  %put NO%str(TE): Dataset &dsin. exists and contains &nobs. observations.;
		  %let &flagname. = 1;
	  %end;
   %end;
   %else %do;
      %put NO%str(TE): Dataset &dsin. does not exist!;
	  %let &flagname. = -1;
   %end;
%mend checkds;
/****************          1 - CHECK_DS - END         ****************/

/****************         2 - IMPORT_CRF_PAGE         ****************/
/*A simple macro to check if a CRF page exists
and import it if yes. Throw error message otherwise*/

%macro import_crf_page
					  (crf_file_path = /*path to the external csv file with crf page*/,
					   crf_page_num  = /*CRF page number*/,
					   dsout         = /*out dataset name*/);

	%let _crf_path = %qsysfunc(compress(&crf_file_path.,%str(%")));
	%let full_fileref = "&_crf_path.\page&crf_page_num..csv";

	%if %sysfunc(fileexist(&full_fileref.)) %then %do;
		%put NO%str(TE): The external file &full_fileref. found. Import Started.;
		PROC IMPORT OUT = &dsout.
			DATAFILE = &full_fileref.
            DBMS     = csv REPLACE;
     		GETNAMES=YES;
	 		guessingrows = max;
		RUN;	
	%end;
	%else %do;		
  		%put ER%str(ROR): The external file &full_fileref. was not found!;
		%abort;
	%end;

%mend import_crf_page;
/****************       2 - IMPORT_CRF_PAGE - END       ****************/


/****************          3 - CALC_PAGE_PARAMS         ****************/
/*A macro to calculate such page parameters as border lengths and space length*/

%macro calc_page_params
					   (dsin  = ,/*input page in a dataset form*/
						dsout = ,/*out dataset name*/
						space_var = space/*macro variable name to keep space value*/);
	
	*Define global macro variables to be used for this page;
	%global b_margin t_margin l_margin r_margin &space_var.;

	*Calculate some page parameters;
	data &dsout.;
		set &dsin.;
		h_endpos = x + width;
		v_endpos = y + height;

		rename x    = h_startpos
			   y    = v_startpos;		
	run;

	*parameters of page;
	proc sql noprint;
		select strip(put(min(v_startpos), best.)), strip(put(max(v_endpos), best.)),
		   	   strip(put(min(h_startpos), best.)), strip(put(max(h_endpos), best.))
		into  :b_margin, :t_margin, :l_margin, :r_margin
		from &dsout.;
	quit;

	*length of space at the page;
	proc sort data = &dsout. out = space;
		by v_endpos;
	run; 

	data space1;
		set space;
		by v_endpos;
		lh_endpos = lag(h_endpos);
		if not First.v_endpos then diff = h_startpos - lh_endpos;
	run;

	*Most frequent value is assumed to be the space;
	proc means data = space1 noprint;
		var diff ;
		output out = _space_len mode = mode;
	run;

	proc sql noprint;
		select strip(put(mode+1, best.)) into :&space_var.
		from _space_len;
	quit;

	%put Page characteristics:;
	%put NO%str(TE): Bottom margin: &b_margin.;
	%put NO%str(TE): Top margin: &t_margin.;
	%put NO%str(TE): Left margin: &l_margin.;
	%put NO%str(TE): Right margin: &r_margin.;

	*Clean up everything except from the out dataset with terms;
	proc datasets lib=work noprint;
		delete space1 _space_len space / memtype=data;
	quit; 

%mend calc_page_params;
/****************       3 - CALC_PAGE_PARAMS - END       ****************/

/****************           4 - CREATE_CRF_DS            ****************/

*Select records that end on position 505. Then merge records into a single record:
those, who is on the same line, but ends not in 505. Consider space length:
Do not merge if space length if big. In other words: merge into a record those, who is
on the same height and has a distance of space between the words;
%macro select_page_tickbox
					(dsin              = ,        /*input dataset with page borders*/
					 space_len         = &space., /*calculated length of space*/
					 tbox_line_end_pos = 505,     /*position of end of line before a tickbox*/
					 corr_tbox_end_pos = 0,       /*number of symbols to define correction end of line before a tickbox position*/
				 	 dsout             = ,        /*out dataset name*/
					);

	proc sort data = &dsin. out = &dsin.1;
		by v_startpos h_endpos;
	run;

	data &dsin.2;
		set &dsin.1;
		by v_startpos ;
		
		length _crf_term CRF_TERM $200 _h_startpos _width 8;

		CRF_TERM = text;		

		retain _crf_term;

		_width = lag(width);
		_h_startpos = lag(h_startpos);

		if first.v_startpos then do;
			_crf_term = crf_term;
			_h_startpos = .;
			_width = .;
			blocks = 1;*this is needed to show how many blocks are on the single line.;
			*a block is the set of words, splitted by space;
		end;
		else do;		
			if _h_startpos + _width + 3*&space_len. ge h_startpos then do;
			/*Use 3 spaces to be sure it's enough*/
				_crf_term = catx(" ", _crf_term, crf_term);
			end;
			else do;
				_crf_term = crf_term;
				blocks+1;
			end;
		end;

		*if Last.v_startpos;
		drop space v_endpos height width _width crf_term _h_startpos text;
	run;

	proc sort data = &dsin.2 out = &dsin.3;
		by v_startpos blocks;
	run;

	*Split by lines that have the tickbox near and others;
	data tickbox
	 	 non_tickbox;
		set &dsin.3;
		by v_startpos blocks;

		if Last.Blocks;

		rename _crf_term = crf_term;

		if &tbox_line_end_pos. - &corr_tbox_end_pos. le h_endpos le &tbox_line_end_pos. + &corr_tbox_end_pos.
			then do;
				*Apply correction;
				h_endpos = &tbox_line_end_pos.;
				output tickbox;
			end;
		else output non_tickbox;

		drop blocks;
	run;

	*Add headers of the sections - they have the same vertical position as other text;
	proc sql noprint;
		create table &dsout. as
		select *, " " as headerfl length = 1 from tickbox
		union
		(select ntb.*, "Y" as headerfl length = 1 from non_tickbox as ntb
		 where ntb.v_startpos in (select distinct v_startpos from tickbox))
		order by v_startpos, h_startpos;	
	quit;

	*Clean up;
	proc datasets lib=work noprint;
		delete &dsin.1 - &dsin.3 tickbox non_tickbox / memtype=data;
	quit;
%mend select_page_tickbox;

%macro select_page_tickbox_headers
		(dsin             = , /*input dataset with CRF terms and header flags*/
		 dsin_prev        = , /*dataset with previous page without headers*/ 
		 dsout            = , /*out dataset*/
		 prev_page_add_fl = );

	*Some CRF fields may be split into multiple pages. In this case;
	*the first block will not have a header. So a previous page;
	*will have to be appended. A flag will indicate if it is needed or not;
	%global &prev_page_add_fl.;
	%let append_fl = ;

	data _null_;
		set &dsin.;
		if _N_ eq 1 and missing(headerfl) then call symput('append_fl', "Y");
	run;

	%checkds(dsin = &dsin_prev., flagname = prev_fl);

	*Add unique numbers for each section and for text lines within each section;
	*Append previous page if required;
	data &dsin.1;
		%if &append_fl. eq Y %then %do;
			%if &prev_fl. le 0 %then %do;
				set &dsin.;
				%let &prev_page_add_fl. = N;
				%put WARN%str(ING): Previous page &dsin_prev. was required to be appended!;
				%put WARN%str(ING): Dataset &dsin_prev. is missing or empty! No dataset appended.;
			%end;
			%else %do;
				set &dsin_prev. &dsin.;
				%put NO%str(TE): Previous page is appended.;
				%let &prev_page_add_fl. = Y;
			%end;
		%end;
		%else %do;
			set &dsin.;
			%let &prev_page_add_fl. = N;
		%end;

		if _N_ eq 1 then section = 0;
		if headerfl eq "Y" then section+1;

		if headerfl eq "Y" then linenum = 0;
		else linenum+1;
	run;

	*Move to vertical structure of headers;
	proc sql noprint;
		create table &dsout. as
		select a.section, a.linenum, b.crf_term as crf_header, a.crf_term from
		&dsin.1 as a join (select section, crf_term from &dsin.1
					  where headerfl eq "Y") as b
		on a.section = b.section
		having b.crf_term ne a.crf_term;
	quit;

	*Clean up;
	proc datasets lib=work noprint;
		delete &dsin.1 / memtype=data;
	quit;
%mend select_page_tickbox_headers;



/****************        4 - CREATE_CRF_DS - END         ****************/

/****************           5 - MERGE_SDTM_CRF            ****************/

/*This macro creates correspondences between the CRF and SDTM data:
both the headers and the text fields. The correspondence is based on
the similarity of words, not the symbols. The process involves many-to-many SQL merges*/
%macro merge_sdtm_crf
					 (dsin_crf  = ,/*input dataset with CRF text fields*/
					  dsin_sdtm = ,/*input dataset with CDISC SDTM data*/
					  dsout     =  /*out dataset name*/);

	proc sql noprint;
		*Select ALL codelist names that fit the headers of sections in the CRF.;
		*Fit means either exact match or section name is a part of a codelist name (and vice versa);
		create table cod_names_headers as
		select distinct cd.codelist_code, cd.codelist_name, pg.*
		from (select distinct section, crf_header from &dsin_crf.) as pg
		cross join &dsin_sdtm. as cd
		where not missing(cd.codelist_code) and compare_strings(cd.codelist_name, pg.crf_header) gt 0
		order by pg.section;

		*Select terms from SDTM that belong to each name from the step above; 
		create table sdtm_terms as
		select cd.*, hdr.section, hdr.crf_header
		from &dsin_sdtm. as cd
		right join cod_names_headers as hdr
		on cd.codelist_code = hdr.codelist_code
		order by hdr.section, cd.SDTM_TERM;

		*Match terms from SDTM to CRF and compute matching scores,;
		* based on similarity by words and whole phrases;
		create table &dsout. as
		select 	st.code, st.codelist_code, st.Codelist_Name, st.SDTM_TERM, st.SDTM_TERM_SYN,
		   	    pg.section, pg.crf_header, pg.linenum, pg.crf_term, pg.CRF_TERM_ORIG,
		   		compare_strings(SDTM_TERM, crf_term) as score_rep,
	      		compare_strings(SDTM_TERM_SYN, crf_term) as score_syn
		from sdtm_terms as st
		cross join &dsin_crf. as pg
		where st.crf_header = pg.crf_header
		order by pg.section, pg.linenum, st.codelist_code;
	quit; 

	*Clean up everything except from the out dataset with terms;
	proc datasets lib=work noprint;
		delete cod_names_headers sdtm_terms / memtype=data;
	quit;

%mend merge_sdtm_crf;
/****************         5 - MERGE_SDTM_CRF - END          ****************/

/****************            6 - KEEP_DROP_PAGE             ****************/
%macro keep_drop_page
				     (dsin      = ,	  /*Input dataset with page*/
				 	  text_cond = ,    /*A condition for keeping or dropping page*/
					  mode      = DROP /*DROP | KEEP: keep page, or drop it if condition is fulfilled*/);

	%if %cmpres(&mode.) eq DROP %then %do;
		%let flag = N;
	%end;
	%else %if %cmpres(&mode.) eq KEEP %then %do;
		%let flag = Y;
	%end;

	data cond_ds;
		length text $200;
		do i = 1 to countw(&text_cond., "q");
			text = scan(&text_cond., i, " ");
			output;
		end;

		drop i;
	run;

	%let count_match = ;
	proc sql noprint;
		select count(distinct cd.text) into :count_match
		from cond_ds as cd
		inner join &dsin. as ds
		on upcase(cd.text) = upcase(ds.text);
	quit;

	%if %cmpres(&mode.) eq DROP %then %do;
		%if %cmpres(&count_match.) eq %cmpres(%sysfunc(countw(&text_cond., "q"))) %then %do; %let flag = D; %end;
		%else %do; %let flag = ND; %end;
	%end;
	%else %if %cmpres(&mode.) eq KEEP %then %do;
		%if %cmpres(&count_match.) eq %cmpres(%sysfunc(countw(&text_cond., "q"))) %then %do; %let flag = K; %end;
		%else %do; %let flag = NK; %end;
	%end;

	%if %cmpres(&flag.) eq D %then %do;
		%put NO%str(TE): All of %unquote(&text_cond.) DROP list values were found in &dsin.!;
		%put NO%str(TE): Page &dsin. removed!;
		proc datasets lib=work noprint;
			delete &dsin. / memtype=data;
		quit;
	%end;
	%else %if %cmpres(&flag.) eq ND %then %do;
		%put NO%str(TE): Not all of %unquote(&text_cond.) DROP list values were found in &dsin.!;
		%put NO%str(TE): Page &dsin. is not removed and considered valid to continue.;
	%end;
	%else %if %cmpres(&flag.) eq K %then %do;
		%put NO%str(TE): All of %unquote(&text_cond.) KEEP list values were found in &dsin.;
		%put NO%str(TE): Page &dsin. is considered valid to continue.;
	%end;
	%else %if %cmpres(&flag.) eq KD %then %do;
		%put NO%str(TE): Not all of %unquote(&text_cond.) KEEP list values were found in &dsin.!;
		%put NO%str(TE): Page &dsin. removed!;
		proc datasets lib=work noprint;
			delete &dsin. / memtype=data;
		quit;
	%end;

	*Clean up;
	proc datasets lib=work noprint;
		delete cond_ds / memtype=data;
	quit; 
%mend keep_drop_page;
/****************         6 - KEEP_DROP_PAGE - END         ****************/


%macro print_test_page(testpg_file_path = ,
					   testpg_pagenum   = );

	%import_crf_page(crf_file_path = &testpg_file_path.,
					 crf_page_num  = &testpg_pagenum.,
					 dsout         = _page)

	%calc_page_params(dsin      = _page,
					  dsout     = _page_borders,
					  space_var = space);

	
	proc sort data = _page_borders out = _page_borders1;
		by h_endpos v_startpos;
		where h_endpos gt 2*(&l_margin. + &r_margin.)/3;
	run;

	proc print data = _page_borders1 noobs label;
		var text h_endpos h_startpos;
		label text       = 'CRF text value'
         	  h_endpos   = 'Horizontal end position of text'
         	  h_startpos = 'Horizontal start position of text';
		title "Text fields that lie on the left 2/3rd part of the page &testpg_pagenum.";
		footnote1 "Search for the text values that are the last within a line and are the closest to the tickbox";
		footnote2 "Compare text to the CRF and find the common horisontal end position. Apply correction if needed."; 
	run;

	*Clean up everything;
	proc datasets lib=work noprint;
		delete _page _page_borders _page_borders1 / memtype=data;
	quit;

	
%mend print_test_page;

%macro match_sdtm_crf_terms(dsin  = ,
					crf_page_num       = ,
				   dsout = );

	**** START RECOGNITION AND MATCHING ****;
	*Step 0: A CRF section can refer to multiple SDTM sections. ;
	*Find sections that have most number of matching terms inside among the others;
	proc sql noprint;
		create table crf_headers_stats as
		select distinct section, crf_header, codelist_code, codelist_name,
		  	   count(distinct crf_term) as count
		from &dsin.
		where /*abs(score_syn*score_rep) ne 0*/ score_syn gt 0 or score_rep gt 0
		group by section, codelist_code
		order by section, count;
	quit;

	data crf_sdtm_headers;
		set crf_headers_stats;	
		by section;

		if Last.section;

		drop count;
	run;

	proc sql noprint;
		create table &dsin.1 as
		select ds.*
		from crf_sdtm_headers as hd
		left join &dsin. as ds
		on hd.section = ds.section and hd.codelist_code = ds.codelist_code
		order by ds.section, ds.linenum, ds.code;
	quit;
	******* END of Step 0 *******;

	*Step 1 of matching. Detect partial or full matching by words only;
	*Classify matching levels:;
	*At this point matching is performed between CRF terms and SDTM terminology/synonyms;
	data full_match
		 part_match_sdtm_in_crf1
		 part_match_sdtm_in_crf2
		 part_match_crf_in_sdtm;
	
		set &dsin.1;

		length warn_msg $1;*Add a flag for warning message for a reviewer to check the matching;

		if score_rep eq 1 or score_syn eq 1
			then output full_match;
		else if score_syn + score_rep eq 24 and upcase(SDTM_TERM) ne upcase(SDTM_TERM_SYN)
			then output part_match_sdtm_in_crf1;
		else if score_syn eq 12 or score_rep eq 12 then do;
			warn_msg = "Y";
			output part_match_sdtm_in_crf2;
		end;
		else if score_syn le 0 and score_rep le 0 then delete;
		else do;
			warn_msg = "Y";
			output part_match_crf_in_sdtm;
		end;
	run;

	*Step 2: Remove CRF terms from part_match_sdtm_in_crf that already completely match;
	*Step 2.5: Remove SDTM terms from part_match_sdtm_in_crf that already completely match;
	*Consider them as FULL match since then, and add to full match;

	*Check what datasets exist;
	%checkds(dsin = full_match,              flagname = full_fl);
	%checkds(dsin = part_match_sdtm_in_crf1, flagname = part_sc1_fl);

	%if &full_fl. eq 1 and &part_sc1_fl. eq 1 %then %do;
		%put NO%str(TE): Step 2: Dataset with full matches and dataset with partial matches are not empty.;
		proc sql noprint;
			create table full_match1 as
			(select * from part_match_sdtm_in_crf1
			where crf_term  not in (select distinct crf_term from full_match) and 
		  		code not in (select distinct code from full_match))
			union (select * from full_match)
			order by SECTION, CODE, LINENUM;
		quit;
	%end;
	%else %if &full_fl. eq 1 and &part_sc1_fl. eq 0 %then %do;
		%put NO%str(TE): Step 2: Dataset with full matches is not empty.;
		%put NO%str(TE): Step 2: Dataset with partial matches is empty!;
		proc sql noprint;
			create table full_match1 as
			select * from full_match
			order by SECTION ,CODE, LINENUM;
		quit;
	%end;
	%else %if &full_fl. eq 0 and &part_sc1_fl. eq 1 %then %do;
		%put NO%str(TE): Step 2: Dataset with full matches is empty!;
		%put NO%str(TE): Step 2: Dataset with partial matches is not empty.;
		proc sql noprint;
			create table full_match1 as
			select * from part_match_sdtm_in_crf1
			order by SECTION, CODE, LINENUM;
		quit;
	%end;
	%else %if &full_fl. eq 0 and &part_sc1_fl. eq 0 %then %do;
		%put NO%str(TE): Step 2: Both datasets with full and partial matches are empty!;
		%put NO%str(TE): Step 2: Dummy dataset created to proceed!;
		data full_match1;
			set &dsin.(where = (0 eq 1));
		run;
	%end;
	*At the end of this step a full_match1 dataset has to be generated;

	*Remove duplicates, generated by sdtm term synonyms;
	proc sort data = full_match1 out = full_match11 nodupkey;
		by section code;
	run;

	*Step 3: Remove CRF terms from part_match_sdtm_in_crf2 that already completely match;
	*Step 3.5: Remove SDTM terms from part_match_sdtm_in_crf2 that already completely match;
	*Consider them as FULL match since then, and add to full match, but ADD A WARNING TO CHECK!;
	%checkds(dsin = full_match11,            flagname = full1_fl);
	%checkds(dsin = part_match_sdtm_in_crf2, flagname = part_sc2_fl);

	%if &full1_fl. eq 1 and &part_sc2_fl. eq 1 %then %do;
		%put NO%str(TE): *** Step 3: Dataset with full matches 1 and dataset with partial matches 2 are not empty. ***;
		proc sql noprint;
			create table full_match2 as
			(select * from part_match_sdtm_in_crf2
			where crf_term  not in (select distinct crf_term  from full_match11) and 
		  		code not in (select distinct code from full_match11))
			union (select * from full_match11)
			order by SECTION, CODE, LINENUM;
		quit;
	%end;
	%else %if &full1_fl. eq 1 and &part_sc2_fl. eq 0 %then %do;
		%put NO%str(TE): Step 3: Dataset with full matches 1 is not empty.;
		%put NO%str(TE): Step 3: Dataset with partial matches 2 is empty!;
		proc sql noprint;
			create table full_match2 as
			select * from full_match11
			order by SECTION, CODE, LINENUM;
		quit;
	%end;
	%else %if &full1_fl. eq 0 and &part_sc2_fl. eq 1 %then %do;
		%put NO%str(TE): Step 3: Dataset with full matches 1 is empty!;
		%put NO%str(TE): Step 3: Dataset with partial matches 2 is not empty.;
		proc sql noprint;
			create table full_match2 as
			select * from part_match_sdtm_in_crf2
			order by SECTION, CODE, LINENUM;
		quit;
	%end;
	%else %if &full1_fl. eq 0 and &part_sc2_fl. eq 0 %then %do;
		%put NO%str(TE): Step 3: Both datasets with full 1 and partial matches 2 are empty!;
		%put NO%str(TE): Step 3: Dummy dataset created to proceed!;
		data full_match2;
			set &dsin.(where = (0 eq 1));
		run;
	%end;
	*At the end of this step a full_match2 dataset has to be generated;

	*Remove duplicates, generated by sdtm term synonyms;
	proc sort data = full_match2 out = full_match22 nodupkey;
		by section code;
	run;

	*Step 4: Remove CRF terms from part_match_crf_in_sdtm that already completely match;
	*Step 4.5: Remove SDTM terms from part_match_crf_in_sdtm that already completely match;
	*Some terms might have FULL match after some pre-processing, ADD A WARNING TO CHECK for others!;
	*This is the last step;
	%checkds(dsin = full_match22,           flagname = full2_fl);
	%checkds(dsin = part_match_crf_in_sdtm, flagname = part_cs_fl);

	%if &full2_fl. eq 1 and &part_cs_fl. eq 1 %then %do;
		%put NO%str(TE): Step 4: Dataset with full matches and dataset with partial matches are not empty.;

		*Remove terms that already have a match. Leave ONLY SDTM terms and their codes;
		proc sql noprint;
			create table full_match_removed as
			select distinct * from part_match_crf_in_sdtm
			where crf_term not in (select distinct crf_term from full_match22) and 
		  		code not in (select distinct code from full_match22)
			order by SECTION, CODE, LINENUM;
		quit;

		*If a CRF term is split on a CRF page then try to combine it;
		%process_partial_match(dsin_part = full_match_removed,
							   dsin_crf  = _crf_page_&crf_page_num.,
                               dsout     = full_match_removed_combine);

		%checkds(dsin = full_match_removed_combine, flagname = comb_fl);
		%put NO%str(TE): SUCCESS! Dataset with full matches created!;

		%if &comb_fl. eq 1 %then %do;			
			data &dsout.;
				set full_match22 full_match_removed_combine;
				drop Codelist_Name SDTM_TERM SDTM_TERM_SYN score_rep score_syn;
			run;
		%end;
		%else %do;
			data &dsout.;
				set full_match22;
				drop Codelist_Name SDTM_TERM SDTM_TERM_SYN score_rep score_syn;
			run;
		%end;

		proc sort data = &dsout.;
			by SECTION LINENUM;
		run;
	%end;

	%else %if &full1_fl. eq 1 and &part_sc2_fl. eq 0 %then %do;
		%put NO%str(TE): Step 4: Dataset with full matches 1 is not empty.;
		%put NO%str(TE): Step 4: Dataset with partial matches is empty.;
		%put NO%str(TE): SUCCESS! Dataset with full matches created!;
		proc sort data = full_match22
				  out  = &dsout.(drop = Codelist_Name SDTM_TERM SDTM_TERM_SYN score_rep score_syn);
			by SECTION LINENUM CODE ;
		run;
	%end;
	%else %if &full1_fl. eq 0 and &part_sc2_fl. eq 1 %then %do;
		%put NO%str(TE): Step 4: Dataset with full matches 1 is empty!;
		%put NO%str(TE): Step 4: Dataset with partial matches 2 is not empty.;

		*If a CRF term is split on a CRF page then try to combine it;
		%process_partial_match(dsin_part = part_match_crf_in_sdtm,
							   dsin_crf  = _crf_page_&crf_page_num.,
                               dsout     = part_match_combine);

		*Check if any matches appeared;
		%checkds(dsin = part_match_combine, flagname = part_comb);

		%if &part_comb. eq 0 %then %do;
			%put WA%str(RNING): Dataset with matches was NOT created! Check CRF terms manualy.;
		%end;
		%else %do;
			*Remove duplicates;
			proc sort data = part_match_combine
					  out = &dsout.(drop = Codelist_Name SDTM_TERM SDTM_TERM_SYN score_rep score_syn) nodupkey;
				by SECTION LINENUM CODE;
			run;
			%put NO%str(TE): SUCCESS! Dataset with full matches created!;
		%end;
	%end;
	%else %if &full1_fl. eq 0 and &part_sc2_fl. eq 0 %then %do;
		%put NO%str(TE): Step 4: Both datasets with full 1 and partial matches 2 are empty!;
		%put WA%str(RNING): Dataset with matches was NOT created! Check CRF terms manualy.;
	%end;

	*Clean up;
	proc datasets lib=work noprint;
		delete &dsin.1 full_match full_match1 full_match2 full_match11 full_match22
			part_match_sdtm_in_crf1 part_match_sdtm_in_crf2 part_match_crf_in_sdtm
			full_match_removed full_match_removed_combine
			part_match_combine crf_headers_stats crf_sdtm_headers / memtype=data;
	quit;
%mend match_sdtm_crf_terms;

%macro check_against_crf(dsin_match    = ,
					     dsin_crf      = ,
						 dsout_nomatch = );
	*Create a separate dataset on the basis of matched terms to see how many;
	*lines per each sections found. Create dataset with non-matches.;

	*Check if dataset with matches exist. If not, then no terms from CRF;
	*found matches in the SDTM; 
	%checkds(dsin = &dsin_match., flagname = match_fl);
	
	%if &match_fl. eq 1 %then %do;
		data &dsin_match.1;			
			set &dsin_match.;
			length linenum 8;

			if not missing(_LINENUM) then do;
				do i = 1 to countw(_linenum);
					linenum = input(scan(_linenum, i, ' '), best.);
					output;
				end;
			end;
			else output;

			drop i _linenum;
		run;

		proc sql noprint;
			create table &dsout_nomatch. as
			select distinct c.SECTION, c.LINENUM, c.CRF_HEADER, c.CRF_TERM, c.CRF_TERM_ORIG
			from &dsin_match.1 as m
			right join &dsin_crf. as c
			on m.SECTION = c.SECTION and m.LINENUM = c.LINENUM
			where missing(m.SECTION) and missing(m.LINENUM);
		quit;

		*Clean up;
		proc datasets lib=work noprint;
			delete &dsin_match.1 / memtype=data;
		quit;
	%end;
	%else %do;
		%put NO%str(TE): Dataset with CRF matches to SDTM is empty or does not exist.;
		data &dsout_nomatch.;
			set &dsin_crf.;
		run;
	%end;
%mend check_against_crf;


%macro generate_code(dsin_match       = ,
					 dsin_nomatch     = ,
					 dsin_sdtm        = ,
					 page_num         = ,
	 				 prev_page_add_fl =   /*Y or N*/,
	  				 prev_page_num    = ,
					 del_perv_page_fl = Y /*Y or N*/,
					 code_path        = );

	*A macro to generate code, based on the full match dataset;

	*Check if dataset with matches exist. Dataset with terms that;
	*do not have a match always exists;
	%checkds(dsin = &dsin_match., flagname = match_fl);
	
	%if &match_fl. eq 1 %then %do;
		proc sql noprint;
			create table prepare_to_output as
			select distinct m.SECTION, m.LINENUM, m.WARN_MSG, m.CRF_TERM_ORIG, m.CRF_HEADER,
			   	s.CDISC_Submission_Value 
			from &dsin_match. as m
			left join &dsin_sdtm. as s
			on m.CODELIST_CODE = s.CODELIST_CODE and m.CODE = s.CODE
			order by m.SECTION, m.LINENUM;
		quit;

		data prepare_to_output1;
			set prepare_to_output &dsin_nomatch.(in = inNM);

			if inNM then WARN_MSG = "M";
		run;
	%end;
	%else %do;
		data prepare_to_output1;
			set &dsin_nomatch.;
			length WARN_MSG $1 CDISC_Submission_Value $1;

			WARN_MSG = "M";
		run;
	%end;

	proc sort data = prepare_to_output1;
		by SECTION LINENUM;
	run;

	%local full_path;
	%local _tmp_path;

	%let _tmp_path = %qsysfunc(compress(&code_path.,%str(%")))\;

	%if &prev_page_add_fl. eq Y %then %do;
		%let full_path = &_tmp_path.code_page&prev_page_num.and&page_num..rtf;
	%end;
	%else %do;
		%let full_path = &_tmp_path.code_page&page_num..rtf;	
	%end;

	filename prevfile "&_tmp_path.code_page&prev_page_num..rtf";

	data _null_;
		set prepare_to_output1 end = LAST;
		by SECTION;

		length _CRF_TERM_ORIG $400;
		_CRF_TERM_ORIG = upcase(CRF_TERM_ORIG);

		file "&full_path";
	
		if _N_ eq 1 then do;			
			%if &prev_page_add_fl. eq Y %then %do;
				put @1 " /*Code generated from page &prev_page_num. appended to page &page_num.*/ ";
				*Delete file with previous page if requested;
				%if &del_perv_page_fl. eq Y %then %do;					
					tmp = fdelete("prevfile");
				%end;
				%else %do;
				%end;				
			%end;
			%else %do;
				put @1 " /*Code generated from page &page_num.*/ ";				
			%end;
			put "proc format; " ;
		end;

		if First.SECTION then put @3 "value <insert_user_value_name>; /* " CRF_HEADER +(-1) " */ ";

		if WARN_MSG eq "Y" then 
			put @6 CRF_TERM_ORIG :$quote. +(-1)" = " CDISC_Submission_Value :$quote. +(-1) "/*Warning: check this line in CRF and SDTM for consistency*/";
		else if WARN_MSG eq "M" then
			put @6 CRF_TERM_ORIG :$quote. +(-1)" = " _CRF_TERM_ORIG :$quote. +(-1) "/*Warning: corresponding value was not found in SDTM!*/";
		else put @6 CRF_TERM_ORIG :$quote. +(-1)" = " CDISC_Submission_Value :$quote.;
    		
		if Last.SECTION then put @3 ";" /;

		if LAST then put @1 "run;" /;
	run;

	*Clean up;
	proc datasets lib=work noprint;
		delete prepare_to_output prepare_to_output1 / memtype=data;
	quit;
%mend generate_code;


/*Prepare the input dataset with SDTM terms*/
/*A macro to split the variable from 1 row into several rows if values are split by separator*/

%macro split_by_sep(dsin      = ,/*input dataset*/
					dsout     = ,/*output dataset*/
					var       = ,/*variable to be splitted*/ 
					separator =  /*separator symbol*/);

	*Fix missing codelist code for the first record in a row issue;
	data &dsin.1;
		set &dsin.;
		if missing(codelist_code) then do;
			codelist_code = code;
			call missing(code);
		end;
	run;

	data &dsin.2;
		set &dsin.1;
		length &var._tmp $200;
		count_sep = countc(&var., &separator.);

		if count_sep gt 0 then do;
			do i = 1 to count_sep+1;
				&var._tmp = strip(scan(&var., i, &separator.));
				output;
			end;
		end;
		else output;
		drop i count_sep;
	run;

	data &dsin.3;
		set &dsin.2;
		if not missing(&var._tmp) then &var. = &var._tmp;
		else;
		
		drop &var._tmp; 
	run;

	*Prepare a dataset with SDTM terms;
	*Replace !@#$%^&*?/\|: symbols with blanks;
	data &dsout.;
		set &dsin.3;
		length SDTM_TERM SDTM_TERM_SYN $400;
		SDTM_TERM = translate(CDISC_Submission_Value, " ", "-!@#$%^&*?/\|:,[]");
		SDTM_TERM_SYN = translate(CDISC_Synonym_s_, " ", "-!@#$%^&*?/\|:,[]");
	run;

	proc datasets lib=work noprint;
		delete &dsin.1 &dsin.2 &dsin.3 / memtype=data;
	run;
%mend split_by_sep;

%macro create_linenum_comb(dsin  = ,
						   dsout = );

	*Read all section numbers into a macro variable and split by space;
	proc sql noprint;
		select distinct section, count(distinct section)
		into :sections separated by '_', :section_count
		from &dsin.;
	quit;

	*Process line numbers within each section;
	%do sec = 1 %to &section_count.;
		proc sql noprint;
			select distinct linenum
			into :linenum separated by '_'
			from &dsin.
			where section = %sysfunc(scan(&sections., &sec., '_'))
			order by linenum desc;
		quit;

		*Generate all possible equences of line numbers in the ascending order of line numbers;
		data tmp&sec.;
			length linenum $200 section 8;	

			section = %sysfunc(scan(&sections., &sec., '_'));
	
			%let line_dim = %sysevalf(%sysfunc(countc("&linenum.", '_'))+1);
			array sect_nums[&line_dim.] val1 - val&line_dim.;

			%do l = 1 %to &line_dim.;
				sect_nums[&l.] = %sysfunc(scan(&linenum., &l., '_'));
			%end;

	    	%do i = 1 %to &line_dim.;
				%do j = 1 %to &i.;
					%if &i. ne &j. %then %do;
							linenum = "_"!!catx("_",of val&i. - val&j.)!!"_";
						output;
					%end;
				%end;
			%end;

			drop val1 - val&line_dim.;
		run;
	%end;

	%let sec_count = %cmpres(&section_count.);

	data &dsout.;
		set tmp1 - tmp&sec_count.;
	run;

	*Clean up;
	proc datasets lib=work noprint;
		delete tmp1 - tmp&sec_count. / memtype=data;
	quit;

%mend create_linenum_comb;


%macro process_partial_match(dsin_part = ,
dsin_crf = ,
dsout = );

proc sql noprint;
			create table rem_full_match_term as
			select distinct SECTION, CODE, CODELIST_CODE, SDTM_TERM, SDTM_TERM_SYN, WARN_MSG
			from &dsin_part.
			where crf_term not in (select distinct crf_term from full_match22) and 
		  		code not in (select distinct code from full_match22)
			order by SECTION, CODE;

			create table rem_full_match_ln as
			select distinct SECTION, LINENUM
			from &dsin_part.
			where crf_term not in (select distinct crf_term from full_match22) and 
		  		  code not in (select distinct code from full_match22)
			order by SECTION, LINENUM;
		quit;

*Create dataset with combinations of linenums that have to be merged;
		%create_linenum_comb(dsin  = rem_full_match_ln,
				             dsout = pm_linenum_comb1);

*Combine CRF text, based on linenum combinations from above;
proc sql noprint;
	create table pm_linenum_comb2 as
	select c.*, p.linenum as _linenum from &dsin_crf. as c
	right join pm_linenum_comb1 as p
	on p.section = c.section
	where index(_linenum, "_"!!strip(put(c.linenum, best.))!!"_") gt 0
	order by _linenum, c.linenum;
quit;

data pm_linenum_comb3;
	set pm_linenum_comb2(drop = linenum);
	by _linenum;

	length _crf_term _crf_term_orig $400 _linenum $200;
	retain _crf_term _crf_term_orig ;

	if First._linenum then do;
		_crf_term = crf_term;
		_crf_term_orig = crf_term_orig;
	end;
	else do;
		_crf_term = catx(" ", _crf_term, crf_term);
		_crf_term_orig = catx(" ", _crf_term_orig, crf_term_orig);
	end;

	crf_term = _crf_term;
	crf_term_orig = _crf_term_orig;

	if Last._linenum;

	_linenum = strip(translate(_linenum, " ", "_"));

	drop _crf_term _crf_term_orig;
run;

*Match terms from SDTM to CRF and compute matching scores,;
* based on similarity by words and whole phrases;
proc sql noprint;
		create table pm_linenum_comb4 as
		select 	ln.crf_header, ln._linenum, ln.crf_term, ln.CRF_TERM_ORIG,
		   	    fm.*,
		   		compare_strings(fm.SDTM_TERM, ln.crf_term) as score_rep,
	      		compare_strings(fm.SDTM_TERM_SYN, ln.crf_term) as score_syn
		from pm_linenum_comb3 as ln
		cross join rem_full_match_term as fm
		where ln.section = fm.section
		order by fm.section, ln._linenum;
quit;

data &dsout.;
	set pm_linenum_comb4;
	if score_syn eq 1 or score_rep eq 1 or
		(score_syn eq 12 and score_rep eq 12 and SDTM_TERM_SYN ne SDTM_TERM);

	drop score_rep score_syn;
run;

*Clean up;
proc datasets lib=work noprint;
	delete rem_full_match_term rem_full_match_ln pm_linenum_comb1 - pm_linenum_comb4 / memtype=data;
quit;

%mend process_partial_match;
