%include 'C:\Users\AArtemchuk\Documents\My SAS Files\9.4\import_macros.sas';

%macro sdtm_check(func_outlib_name       = ,
						  dsin_sdtm              = ,
                          variable_list          = ,
						  sdtm_term_xlsfile_path = ,
					      sdtm_term_sheet_name   = ,
						  report_file_path       = );

	%search_text_addfunc(outlib = &func_outlib_name.);

	options cmplib = %sysfunc(scan("&func_outlib_name.", 1, ".")).%sysfunc(scan("&func_outlib_name.", 2, "."));

	proc import out      = cdisc_sdtm_terms_tmp
				datafile = &sdtm_term_xlsfile_path.
            	dbms     = xls replace;
     	getnames = yes;
	 	sheet    = &sdtm_term_sheet_name.;
	run;

	%checkds(dsin = cdisc_sdtm_terms_tmp, flagname = sdtm_ds_fl);
	%if &sdtm_ds_fl. lt 1 %then %do;
		%put WARN%str(ING): Dataset with SDTM terms is blank or missing! Checking is stopped!;
		%goto alg_stop;	
	%end;
	%else %do;
		%put NO%str(TE): Dataset with SDTM terms found.;
	%end;

	*Fix missing codelist code for the first record in a row issue;
	data cdisc_sdtm_terms;
		length CDISC_SUBMISSION_VALUE $200;
		set cdisc_sdtm_terms_tmp;
		if missing(codelist_code) then do;
			codelist_code = code;
			call missing(code);
		end;

		rename VAR3 = Codelist_ext;
	run;

	proc sort data = cdisc_sdtm_terms out = cdisc_sdtm_headers(keep = codelist_code codelist_name) nodupkey;
		by codelist_code;
	run;		

	%if &variable_list. ne %str() %then %do;

		%do i = 1 %to %sysfunc(countw(&variable_list.));

			%let var&i. = %sysfunc(scan(&variable_list., &i., " "));
			%let var_label&i. = ;

			*Check if specified variable exists in the dataset;
			%let dsid = %sysfunc(open(&dsin_sdtm.));

			%if (&dsid.) %then %do;
  				%if %sysfunc(varnum(&dsid., &&var&i.)) gt 0 %then %do;
					%put NO%str(TE): Variable &&var&i. was found in &dsin_sdtm.;
				%end;
  				%else %do;
					%put WARN%str(ING): Variable &&var&i. was not found in &dsin_sdtm! Checking is stopped!;
					%goto exit;
				%end;
 				%let rc = %sysfunc(close(&dsid));
			%end;

			data _null_;
				set &dsin_sdtm.(where = (not missing(&&var&i.)));

				call symput("var_label&i.", vlabel(&&var&i.));			
			run;

			%if &&var_label&i. eq %str() %then %do;
				%put WARN%str(ING): Variable &&var&i. does not have a label! Checking is stopped!;
				%goto exit;
			%end;

			data section_score_&&var&i.;
				set cdisc_sdtm_headers;
				call compcost('APPEND='     , 50,
							  'BLANK='      , 50, 
							  'DELETE='     , 75,
							  'DOUBLE='     , 200,
							  'FDELETE='    , 100,
							  'FINSERT='    , 200,
							  'FREPLACE='   , 500,
							  'INSERT='     , 75,
							  'MATCH='      , 0,
							  'PUNCTUATION=', 100,
							  'REPLACE='    , 500,
							  'SINGLE='     , 100,
							  'SWAP='       , 500,
							  'TRUNCATE='   , 50);

				score = compged(codelist_name, "&&var_label&i.");
			run;

			proc sort data = section_score_&&var&i.;
				by score;
			run;

			data section_match_&&var&i.;
				set section_score_&&var&i.;
				by score;
				if _N_ eq 1;
				keep codelist_code;
			run;

			proc sql noprint;
				create table match_terms_&&var&i. as
				select * from cdisc_sdtm_terms
				where codelist_code in (select codelist_code from section_match_&&var&i.)
				order by CDISC_SUBMISSION_VALUE;

				create table dsin_sdtm_&&var&i. as
				select distinct &&var&i. as CDISC_SUBMISSION_VALUE length = 200
				from &dsin_sdtm.
				order by CDISC_SUBMISSION_VALUE;
			quit;

			%local full_path;
			%local _tmp_path;

			%let _tmp_path = %qsysfunc(compress(&report_file_path.,%str(%")))\;
			%let full_path = &_tmp_path.report_&&var&i...rtf;

			data prep_to_print_&&var&i.;
				merge match_terms_&&var&i.(in = inMA) dsin_sdtm_&&var&i.(in = inDS);
				by CDISC_SUBMISSION_VALUE;

				length _Codelist_ext $3;

				retain _Codelist_ext;

				if _N_ eq 1 then _Codelist_ext = Codelist_ext;
				else Codelist_ext = _Codelist_ext;

				file "&full_path.";

				if _N_ eq 1 then do;
					put @1 "Results of checking for variable &&var&i. (&&var_label&i.) mapped values against CDISC SDTM" /;
					put @1 "Value:" @34 "Check result:";
				end;

				if inMA and inDS then do;
					put @2 CDISC_SUBMISSION_VALUE @35 "OK - mapping is correct";
				end;
				else if inDS and upcase(Codelist_ext) eq "YES" then do;
					put @2 CDISC_SUBMISSION_VALUE @35 "OK - list of terms is extensible";
				end;
				else if inDS and upcase(Codelist_ext) eq "NO" then do;
					put @2 CDISC_SUBMISSION_VALUE @35 "Not found in the SDTM Terminology!";
				end;
			run;
			%exit:	

			*Clean up;
			proc datasets lib=work noprint;
				delete prep_to_print_&&var&i. match_terms_&&var&i. dsin_sdtm_&&var&i. 
					   section_score_&&var&i. section_match_&&var&i. / memtype=data;
			quit; 
		%end;
	%end;
	%else %do;
		%put WARN%str(ING): Variable list was not specified! Checking was stopped!;
		%goto alg_stop;	
	%end;

	*Clean up;
	proc datasets lib=work noprint;
		delete cdisc_sdtm_terms cdisc_sdtm_terms_tmp cdisc_sdtm_headers userfuncs / memtype=data;
	quit; 

	%alg_stop:

%mend sdtm_check;

%sdtm_check(func_outlib_name       = <lib.ds_name.user_name for function>,
            dsin_sdtm              = <SDTM dataset lib.name>,
            variable_list          = <space-delimited list of variables>,
		     sdtm_term_xlsfile_path = "<SDTM Controlled Terminology file path>",
		     sdtm_term_sheet_name   = "<SDTM Controlled Terminology Excel sheet>",
		     report_file_path       = "<Path to output files with report>");

