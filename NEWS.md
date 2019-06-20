; phuse_0.1.8 (Release Date: 06/30/2019)
# ---------------------------------------------------------------------------
* added search_api, start_app, clone_github
* updated 02-display/app.R

; phuse_0.1.7 (Release Date: 12/31/2018)
# ---------------------------------------------------------------------------
* added is_empty, echo_msg, search_github

; phuse_0.1.6 (Release Date: 09/15/2018)
# ---------------------------------------------------------------------------
* Updated get_inputs
* fixed a bug reported by CRAN maintainer 

; phuse_0.1.5 (Release Date: 3/25/2018)
# ---------------------------------------------------------------------------
* Added get_inputs, url.exists in build_script_df
* Added examples 03_showenv, o4_merge, 05_d3
* Updated chk_workdir, run_example, merge_lists, crt_workdir, create_dir,
  get_yml_inputs, read_yml, start_phuse

; phuse_0.1.4 (Release Date: 9/25/2017)
# ---------------------------------------------------------------------------
* Added start_phuse, cvt_class2df
* Added examples 03_showenv
* Updated examples 02_display

; phuse_0.1.3 (Release Date: 9/20/2017)
# ---------------------------------------------------------------------------
* Added get_yml_inputs and get_inputs
* Test the interactive web inputs
* Added the Script, YML, Info, Metadata, Verify, Merge, Download and Execute
  tabs in 02_display example
* Removed test_init

; phuse_0.1.2 (Release Date: 9/17/2017)
# ---------------------------------------------------------------------------
* Added build_script_df
* Test the repository clone
* Build script index file

; phuse_0.1.1 (Release Date: 9/15/2017)
# ---------------------------------------------------------------------------
* Added examples: 04_merge, 01_html and 02_display
* Added cvt_list2df, merge_list, extract_fns and download_fns
* Added crt_workdir, chk_workdir, build_inputs

; phuse_0.1.0 (Release Date: 8/28/2017)
# ---------------------------------------------------------------------------
* Added create_dir, download_script, init_cfg and test_init
* Added read_yml, resolve and download_script_files
* 1st time to build the package

; Steps to publish package to CRAN 
# ---------------------------------------------------------------------------
cd /Users/htu/Desktop/myGithub/pkgs
R CMD build phuse/trunk
R CMD check --as-cran

