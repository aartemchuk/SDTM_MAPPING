
%include 'C:\Users\AArtemchuk\Documents\My SAS Files\9.4\import_macros.sas';


%macro auto_mapping(func_outlib_name = ,
					sdtm_term_xlsfile_path = ,
					sdtm_term_sheet_name = ,
					crf_page_path = ,
					print_test_pagenum = ,
					keep_or_drop_text = ,
					keep_or_drop_mode = ,
					start_pagenum = ,
					end_pagenum = ,
					tbox_line_end    = ,
					corr_tbox_end    = ,
					del_prev_page_fl = ,
					output_code_path = ,
					show_temp_datasets = );

	%search_text_addfunc(outlib = &func_outlib_name.);

	options cmplib = %sysfunc(scan("&func_outlib_name.", 1, ".")).%sysfunc(scan("&func_outlib_name.", 2, "."));

	proc import out      = cdisc_sdtm_terms_tmp
				datafile = &sdtm_term_xlsfile_path.
            	dbms     = xls replace;
     	getnames = yes;
	 	sheet    = &sdtm_term_sheet_name.;
	run;

	%split_by_sep(dsin      = cdisc_sdtm_terms_tmp,
			  	  dsout     = cdisc_sdtm_terms,
			  	  var       = CDISC_Synonym_s_,
			  	  separator =  ";");

	%if &print_test_pagenum. gt 0 %then %do;
		%print_test_page(testpg_file_path = &crf_page_path.,
					     testpg_pagenum   = &print_test_pagenum.);
		%goto exit;
	%end;

	%do pagenum = &start_pagenum. %to &end_pagenum.;
		%import_crf_page
	 	(crf_file_path      = &crf_page_path.,
		 crf_page_num       = &pagenum.,
		 dsout              = page&pagenum.);

		%keep_drop_page
	 	(dsin               = page&pagenum.,
	 	 text_cond          = &keep_or_drop_text.,
	  	 mode               = &keep_or_drop_mode.);

		%checkds(dsin       = page&pagenum.,
                 flagname   = page&pagenum._fl);

		%if &&page&pagenum._fl lt 1 %then %do;
			%put NO%str(TE): End of processing page &pagenum.! Go to next page;
			%goto exit;
		%end;

		%calc_page_params
	 	(dsin               = page&pagenum.,
	  	 dsout              = borders_page&pagenum.,
	  	 space_var          = space);

		%select_page_tickbox
	 	(dsin               = borders_page&pagenum.,  
	  	 space_len          = &space.,
	  	 tbox_line_end_pos  = &tbox_line_end.,
	  	 corr_tbox_end_pos  = &corr_tbox_end.,
	  	 dsout              = nohead_page&pagenum.);

		%let pr_pagenum = %sysevalf(&pagenum. - 1);

		%select_page_tickbox_headers
	 	(dsin               = nohead_page&pagenum.,
     	 dsin_prev          = nohead_page&pr_pagenum.,
	  	 dsout              = crf_page&pagenum.,
	 	 prev_page_add_fl   = append_fl);

		%if &append_fl. eq Y %then %do;
			%put NO%str(TE): Previous page &pr_pagenum. was appended to page &pagenum.;
			%put NO%str(TE): Previous page &pr_pagenum. is no longer needed.;
		%end;

		%checkds(dsin       = crf_page&pagenum.,
                 flagname   = crf_page&pagenum._fl);

		%if &&crf_page&pagenum._fl lt 1 %then %do;
			%put NO%str(TE): End of processing page &pagenum.! Go to next page;
			%goto exit;
		%end;

		data _crf_page_&pagenum.;
			set crf_page&pagenum.;

			length CRF_TERM_ORIG $400;
			*replace !@#$%^&*?/\|: symbols with blanks;
			CRF_TERM_ORIG = CRF_TERM;
			CRF_TERM = translate(CRF_TERM, " ", "/-!@#$%^&*?\|:,;");
		run;

		%merge_sdtm_crf
	 	(dsin_crf           = _crf_page_&pagenum.,
	  	 dsin_sdtm          = cdisc_sdtm_terms,
	  	 dsout              = crf_sdtm_page&pagenum.);

		%match_sdtm_crf_terms
	 	(dsin               = crf_sdtm_page&pagenum.,
		 crf_page_num       = &pagenum.,
	 	 dsout              = match_page&pagenum.);

		%check_against_crf
	 	(dsin_match         = match_page&pagenum.,
	  	 dsin_crf           = _crf_page_&pagenum.,
	  	 dsout_nomatch      = no_match_page&pagenum.);

		%generate_code
	 	(dsin_match         = match_page&pagenum.,
	 	 dsin_nomatch       = no_match_page&pagenum.,
	  	 dsin_sdtm          = cdisc_sdtm_terms,
	 	 page_num           = &pagenum.,
	   	 prev_page_add_fl   = &append_fl., /*Y or N*/
	  	 prev_page_num      = &pr_pagenum.,
	 	 del_perv_page_fl   = &del_prev_page_fl., /*Y or N*/
	  	 code_path          = &output_code_path.);

		%exit:

		%if &show_temp_datasets. eq N %then %do;
			*Clean up;
			proc datasets lib=work noprint;
				delete page&pagenum. borders_page&pagenum. nohead_page&pr_pagenum. crf_page&pagenum. _crf_page_&pagenum. 
			   		   crf_sdtm_page&pagenum. match_page&pagenum. no_match_page&pagenum. cdisc_sdtm_terms_tmp / memtype=data;
			quit;
		%end;
	%end;

	*Clean up;
	proc datasets lib=work noprint;
		delete cdisc_sdtm_terms userfuncs / memtype=data;
	quit;

%mend auto_mapping;

options mprint symbolgen;

%auto_mapping(func_outlib_name       = work.userfuncs.char,
			  sdtm_term_xlsfile_path = "c:\Users\AArtemchuk\Documents\tmp\SDTM_Terminology.xls",
			  sdtm_term_sheet_name   = "SDTM Terminology 2019-06-28",
			  crf_page_path          = "c:\Users\AArtemchuk\Documents\tmp",
			  print_test_pagenum     = ,
			  keep_or_drop_text      = "field name label pre-filled values",/*do not repeat values*/
			  keep_or_drop_mode      = DROP, /*KEEP | DROP*/
			  start_pagenum          = 60,
			  end_pagenum            = 62,
			  tbox_line_end          = 505,
			  corr_tbox_end          = 3,
			  del_prev_page_fl       = Y,
			  output_code_path       = "c:\Users\AArtemchuk\Documents\tmp",
			  show_temp_datasets     = Y);
