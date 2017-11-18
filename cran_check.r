N  checking R code for possible problems
partitionRACVM: no visible global function definition for 'stripModel'
Undefined global functions or variables:
  stripModel
???  checking Rd files
W  checking Rd metadata
Rd files with duplicated alias 'plotPhaseParameter':
  'getPhaseParameter.Rd' 'plotPhaseParameter.Rd'
???  checking Rd line widths
???  checking Rd cross-references
???  checking for missing documentation entries
W  checking for code/documentation mismatches
Codoc mismatches from documentation object 'simulateUCVM':
  simulateUCVM
-    Code: function(nu = 1, tau = 1, v0 = nu * exp((0+1i) * runif(1, 0, 2
                                                                  -                   * pi)), 
                    T.max = NULL, dt = NULL, T = NULL, method =
                      c("direct", "exact")[1])
  -    Docs: function(nu = 1, tau = 1, v0 = nu * exp((0 + (0+1i)) * runif(1,
                                                                          -                   0, 2 * pi)), 
                      T.max = NULL, dt = NULL, T = NULL, method
                      = c("direct", "exact")[1])
    Mismatches in argument default values:
  -      Name: 'v0' Code: nu * exp((0+1i) * runif(1, 0, 2 * pi)) Docs: nu * exp((0 + (0+1i)) * runif(1, 0, 2 * pi))

Codoc mismatches from documentation object 'simulateUCVM.exact':
  simulateUCVM.exact
-    Code: function(T, nu = 1, tau = 1, v0 = nu * exp((0 + (0+1i)) *
                                                        -                   runif(1, 0, 2 * pi)))
  -    Docs: function(T, nu = 1, tau = 1, v0 = nu * exp((0 + (0 + (0+1i))) *
                                                          -                   runif(1, 0, 2 * pi)))
    Mismatches in argument default values:
  -      Name: 'v0' 
Code: nu * exp((0 + (0 + 1i)) * runif(1, 0, 2 * pi)) 
Docs: nu * exp((0 + (0 + (0 + 1i))) * runif(1, 0, 2 * pi))

Data codoc mismatches from documentation object 'Kestrel':
  Variables in data frame 'Kestrel'
Code: ID X Y event.id individual.taxon.canonical.name latitude
longitude manually.marked.outlier sensor.type study.name
tag.local.identifier timestamp visible
Docs: Time event.id visible

W  checking Rd \usage sections
Undocumented arguments in documentation object 'findSingleBreakPoint'
'T'

Duplicated \argument entries in documentation object 'getPhaseParameter':
  'variable' 'partitionlist'

Undocumented arguments in documentation object 'getV.spline'
'T.new'
Documented arguments not in \usage in documentation object 'getV.spline':
  'T.spline'

Undocumented arguments in documentation object 'plotPhaseParameter'
'cols' 'extra'

Undocumented arguments in documentation object 'testCP'
'T'

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter 'Writing R documentation files' in the 'Writing R
Extensions' manual.
W  checking Rd contents
Argument items with no description in Rd object 'findCandidateChangePoints':
  'windowsweep' 'clusterwidth'

Argument items with no description in Rd object 'findSingleBreakPoint':
  'Z' 'plotme' 'method' '...'

Argument items with no description in Rd object 'summarizePhases':
  'phases'

Argument items with no description in Rd object 'testCP':
  'criterion' 'spline'

W  checking for unstated dependencies in examples
'library' or 'require' call not declared from: 'gridExtra'
???  checking contents of 'data' directory
???  checking data for non-ASCII characters
???  checking data for ASCII and uncompressed saves
???  checking line endings in C/C++/Fortran sources/headers
???  checking line endings in Makefiles
???  checking compilation flags in Makevars
???  checking for GNU extensions in Makefiles
???  checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS)
???  checking compiled code
W  checking files in 'vignettes'
Files in the 'vignettes' directory but no files in 'inst/doc':
  'bibliography.rtf', 'bowheadsweep2.robj',
'cache/EVAFplots_cf5bff833fbd05b56cd93c6a29883b2b.RData',
'cache/EVAFplots_cf5bff833fbd05b56cd93c6a29883b2b.rdb',
'cache/EVAFplots_cf5bff833fbd05b56cd93c6a29883b2b.rdx',
'cache/FirstPlot_0f14d474eae8a974352757bb6fd2c541.RData',
'cache/FirstPlot_0f14d474eae8a974352757bb6fd2c541.rdb',
'cache/FirstPlot_0f14d474eae8a974352757bb6fd2c541.rdx',
'cache/K1.fit_27b3f2799d9cccb430004c09490ed6ca.RData',
'cache/K1.fit_27b3f2799d9cccb430004c09490ed6ca.rdb',
'cache/K1.fit_27b3f2799d9cccb430004c09490ed6ca.rdx',
'cache/K1.sim_857816a4702f9e647b00824b3bb8e71a.RData',
'cache/K1.sim_857816a4702f9e647b00824b3bb8e71a.rdb',
'cache/K1.sim_857816a4702f9e647b00824b3bb8e71a.rdx',
'cache/K1_8bd6435c07d4c1a3c3f1211d5ec06ccc.RData',
'cache/K1_8bd6435c07d4c1a3c3f1211d5ec06ccc.rdb',
'cache/K1_8bd6435c07d4c1a3c3f1211d5ec06ccc.rdx',
'cache/TwoPhaseCVM_bf394aad48fac545b202ad3dd2481ee7.RData',
'cache/TwoPhaseCVM_bf394aad48fac545b202ad3dd2481ee7.rdb',
'cache/TwoPhaseCVM_bf394aad48fac545b202ad3dd2481ee7.rdx',
'cache/VAF.lores.splined_d75f96b6a5ccd30e33bd3e9bf96dc935.RData',
'cache/VAF.lores.splined_d75f96b6a5ccd30e33bd3e9bf96dc935.rdb',
'cache/VAF.lores.splined_d75f96b6a5ccd30e33bd3e9bf96dc935.rdx',
'cache/VAF.lores_ebb0d8c6c121789af844713f5246d830.RData',
'cache/VAF.lores_ebb0d8c6c121789af844713f5246d830.rdb',
'cache/VAF.lores_ebb0d8c6c121789af844713f5246d830.rdx',
'cache/VAF1_722a185ba69fdb27026c04fbb4c01e86.RData',
'cache/VAF1_722a185ba69fdb27026c04fbb4c01e86.rdb',
'cache/VAF1_722a185ba69fdb27026c04fbb4c01e86.rdx',
'cache/__packages',
'cache/acvm.fit_2fac933d1659f0ef0769350627e8a349.RData',
'cache/acvm.fit_2fac933d1659f0ef0769350627e8a349.rdb',
'cache/acvm.fit_2fac933d1659f0ef0769350627e8a349.rdx',
'cache/acvm_762fe513d06cf386db74afb3f53fbc0c.RData',
'cache/acvm_762fe513d06cf386db74afb3f53fbc0c.rdb',
'cache/acvm_762fe513d06cf386db74afb3f53fbc0c.rdx',
'cache/crawl1_3ffa3bc80b9a996ffa5f8d9e65b4a3ea.RData',
'cache/crawl1_3ffa3bc80b9a996ffa5f8d9e65b4a3ea.rdb',
'cache/crawl1_3ffa3bc80b9a996ffa5f8d9e65b4a3ea.rdx',
'cache/crawl2_250b4a5741dc6a9e538c7ef7af86f995.RData',
'cache/crawl2_250b4a5741dc6a9e538c7ef7af86f995.rdb',
'cache/crawl2_250b4a5741dc6a9e538c7ef7af86f995.rdx',
'cache/exampleCPtables_811213ab0bf3d2e8faa54c900320e751.RData',
'cache/exampleCPtables_811213ab0bf3d2e8faa54c900320e751.rdb',
'cache/exampleCPtables_811213ab0bf3d2e8faa54c900320e751.rdx',
'cache/filterKestrelCPs_a1d24cc9546c9e2c93cf77fc7c2381e2.RData',
'cache/filterKestrelCPs_a1d24cc9546c9e2c93cf77fc7c2381e2.rdb',
'cache/filterKestrelCPs_a1d24cc9546c9e2c93cf77fc7c2381e2.rdx',
'cache/findKestrelCPs_90549cca636e368a8c9e6d405f97d2be.RData',
'cache/findKestrelCPs_90549cca636e368a8c9e6d405f97d2be.rdb',
'cache/findKestrelCPs_90549cca636e368a8c9e6d405f97d2be.rdx',
'cache/kestrelMegaPlot_44a045b1ec3dd2ec4101527771b1456b.RData',
'cache/kestrelMegaPlot_44a045b1ec3dd2ec4101527771b1456b.rdb',
'cache/kestrelMegaPlot_44a045b1ec3dd2ec4101527771b1456b.rdx',
'cache/plotKestrelSweep_f42aecc863a85adb49b870f372f04175.RData',
'cache/plotKestrelSweep_f42aecc863a85adb49b870f372f04175.rdb',
'cache/plotKestrelSweep_f42aecc863a85adb49b870f372f04175.rdx',
'cache/plotRACVM_3fbdf1c4ce0d445b05e7097c62f1fa8a.RData',
'cache/plotRACVM_3fbdf1c4ce0d445b05e7097c62f1fa8a.rdb',
'cache/plotRACVM_3fbdf1c4ce0d445b05e7097c62f1fa8a.rdx',
'cache/racvm.fit_e07923850d985896bd5eba634c3153a3.RData',
'cache/racvm.fit_e07923850d985896bd5eba634c3153a3.rdb',
'cache/racvm.fit_e07923850d985896bd5eba634c3153a3.rdx',
'cache/racvm_d96b150e3ae2a2613810015e6d632d95.RData',
'cache/racvm_d96b150e3ae2a2613810015e6d632d95.rdb',
'cache/racvm_d96b150e3ae2a2613810015e6d632d95.rdx',
'cache/rcvm.fit_e2a73a486df2b934fd89dae65685f8e4.RData',
'cache/rcvm.fit_e2a73a486df2b934fd89dae65685f8e4.rdb',
'cache/rcvm.fit_e2a73a486df2b934fd89dae65685f8e4.rdx',
'cache/rcvm_50cb8939f82f0189ba95d371690960c3.RData',
'cache/rcvm_50cb8939f82f0189ba95d371690960c3.rdb',
'cache/rcvm_50cb8939f82f0189ba95d371690960c3.rdx',
'cache/simGetCPtable2_b97922d71a004ec52daafc8fa648e47b.RData',
'cache/simGetCPtable2_b97922d71a004ec52daafc8fa648e47b.rdb',
'cache/simGetCPtable2_b97922d71a004ec52daafc8fa648e47b.rdx',
'cache/simSweep1_6ecadf5ec03797f64efc6b8c61d819a2.RData',
'cache/simSweep1_6ecadf5ec03797f64efc6b8c61d819a2.rdb',
'cache/simSweep1_6ecadf5ec03797f64efc6b8c61d819a2.rdx',
'cache/simSweepAllModels_7e3cc42ddbc9d154e9c8e72dfe83c538.RData',
'cache/simSweepAllModels_7e3cc42ddbc9d154e9c8e72dfe83c538.rdb',
'cache/simSweepAllModels_7e3cc42ddbc9d154e9c8e72dfe83c538.rdx',
'cache/simulateRACVM_e425c12a7835241078331139671ded6a.RData',
'cache/simulateRACVM_e425c12a7835241078331139671ded6a.rdb',
'cache/simulateRACVM_e425c12a7835241078331139671ded6a.rdx',
'cache/sweepKestrel_ef9292c12b900ce0eab600eaa29385f4.RData',
'cache/sweepKestrel_ef9292c12b900ce0eab600eaa29385f4.rdb',
'cache/sweepKestrel_ef9292c12b900ce0eab600eaa29385f4.rdx',
'cache/ucvm.exact_397192e1474f6c4876b54d664ce21e6a.RData',
'cache/ucvm.exact_397192e1474f6c4876b54d664ce21e6a.rdb',
'cache/ucvm.exact_397192e1474f6c4876b54d664ce21e6a.rdx',
'cache/ucvm.fit_78fd5456c5ae55b9909111d63763e322.RData',
'cache/ucvm.fit_78fd5456c5ae55b9909111d63763e322.rdb',
'cache/ucvm.fit_78fd5456c5ae55b9909111d63763e322.rdx',
'cache/ucvm1_61494f57be4b743f05894790e642bc7a.RData',
'cache/ucvm1_61494f57be4b743f05894790e642bc7a.rdb',
'cache/ucvm1_61494f57be4b743f05894790e642bc7a.rdx',
'cache/ucvm_45cb7c49776d2678491d5b554057a907.RData',
'cache/ucvm_45cb7c49776d2678491d5b554057a907.rdb',
'cache/ucvm_45cb7c49776d2678491d5b554057a907.rdx',
'cache/unnamed-chunk-9_21ecbbfc0aa34d028d60e408fed8ba9a.RData',
'cache/unnamed-chunk-9_21ecbbfc0aa34d028d60e408fed8ba9a.rdb',
'cache/unnamed-chunk-9_21ecbbfc0aa34d028d60e408fed8ba9a.rdx',
'cache/vLike.irregular_aedd9ab56aa6edb523ae42607ff9fb17.RData',
'cache/vLike.irregular_aedd9ab56aa6edb523ae42607ff9fb17.rdb',
'cache/vLike.irregular_aedd9ab56aa6edb523ae42607ff9fb17.rdx',
'cache/vLike_c0638df620c57e2fb65dec7fda9163ea.RData',
'cache/vLike_c0638df620c57e2fb65dec7fda9163ea.rdb',
'cache/vLike_c0638df620c57e2fb65dec7fda9163ea.rdx',
'cache/zLike.irregular_0f200aa08dab169d6083e363cb84c830.RData',
'cache/zLike.irregular_0f200aa08dab169d6083e363cb84c830.rdb',
'cache/zLike.irregular_0f200aa08dab169d6083e363cb84c830.rdx',
'figure/CRW-1.pdf', 'figure/CRW-2.pdf', 'figure/EVAFplots-1.pdf',
'figure/FirstPlot-1.pdf', 'figure/K1-1.pdf', 'figure/K1_sim-1.pdf',
'figure/KestrelFlight-1.pdf', 'figure/TwoPhaseCVM-1.pdf',
'figure/VAF1-1.pdf', 'figure/acvm-1.pdf',
'figure/kestrelMegaPlot-1.pdf', 'figure/plotKestrelSweep-1.pdf',
'figure/plotRACVM-1.pdf', 'figure/racvm-1.pdf',
'figure/rcvm-1.pdf', 'figure/simSweepGenerate-1.pdf',
'figure/subsetKestrel-1.pdf', 'figure/ucvm-1.pdf',
'figure/ucvm_exact-1.pdf', 'figure/unnamed-chunk-11-1.pdf',
'figure/unnamed-chunk-14-1.pdf', 'figure/unnamed-chunk-18-1.pdf',
'k.sweep.robj', 'smoove-concordance.tex', 'smoove.Rnw',
'smoove.log', 'smoove.pdf', 'smoove.synctex.gz', 'smoove.tex',
'smoove.toc'
The following files look like leftovers/mistakes:
  'smoove.log', 'smoove.toc'
Please remove them from your package.
The following directories look like leftovers from 'knitr':
  'cache', 'figure'
Please remove from your package.
-  checking examples ...
E  ** running examples for arch 'i386'
Running examples in 'smoove-Ex.R' failed
The error most likely occurred in:
  
  > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: estimateUCVM
  > ### Title: Estimating parameters of unbiased CVM
  > ### Aliases: estimateUCVM
  > 
  -  > ### ** Examples
  > 
  > require(smoove)
> 
  > #----------------------------------------------------------
> # Example 1: VAF method (high resolution, regular sampling)
  > #----------------------------------------------------------
> 
  >   nu <- 10
>   tau <- 3
>   ucvm1 <- simulateUCVM(nu=nu, tau = tau, dt = 0.1, T.max = 1000)	
>   plot(ucvm1$Z, asp=1, cex=0.5, pch=19, col=rgb(0,0,0,.1))
>   estimateUCVM(Z = ucvm1$Z, t = ucvm1$T, CI=TRUE, method="vaf", diagnose=TRUE)
Error in estimateCVM.vaf(Z, T, CI = CI, spline = spline, diagnose = diagnose,  : 
                           unused argument (t = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4, 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 6, 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 7, 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8, 8.1, 8.2, 8.3, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 9, 9.1, 9.2, 9.3, 9.4, 9.5, 9.6, 9.7, 9.8, 9.9, 10, 10.1, 
                                                  10.2, 10.3, 10.4, 10.5, 10.6, 10.7, 10.8, 10.9, 11, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8, 11.9, 12, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 12.8, 12.9, 13, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 13.8, 13.9, 14, 14.1, 14.2, 14.3, 14.4, 14.5, 14.6, 14.7, 14.8, 14.9, 15, 15.1, 15.2, 15.3, 15.4, 15.5, 15.6, 15.7, 15.8, 15.9, 16, 16.1, 16.2, 16.3, 16.4, 16.5, 16.6, 16.7, 16.8, 16.9, 17, 17.1, 17.2, 17.3, 17.4, 17.5, 17.6, 17.7, 17.8, 17.9, 18, 18.1, 18.2, 18.3, 
                                                  Calls: estimateUCVM
                                                  Execution halted
                                                  E  ** running examples for arch 'x64'
                                                  Running examples in 'smoove-Ex.R' failed
                                                  The error most likely occurred in:
                                                    
                                                    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
                                                  > ### Name: estimateUCVM
                                                    > ### Title: Estimating parameters of unbiased CVM
                                                    > ### Aliases: estimateUCVM
                                                    > 
                                                    -  > ### ** Examples
                                                    > 
                                                    > require(smoove)
                                                  > 
                                                    > #----------------------------------------------------------
                                                  > # Example 1: VAF method (high resolution, regular sampling)
                                                    > #----------------------------------------------------------
                                                  > 
                                                    >   nu <- 10
                                                  >   tau <- 3
                                                  >   ucvm1 <- simulateUCVM(nu=nu, tau = tau, dt = 0.1, T.max = 1000)	
                                                  >   plot(ucvm1$Z, asp=1, cex=0.5, pch=19, col=rgb(0,0,0,.1))
                                                  >   estimateUCVM(Z = ucvm1$Z, t = ucvm1$T, CI=TRUE, method="vaf", diagnose=TRUE)
                                                  Error in estimateCVM.vaf(Z, T, CI = CI, spline = spline, diagnose = diagnose,  : 
                                                                             unused argument (t = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4, 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 6, 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 7, 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8, 8.1, 8.2, 8.3, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 9, 9.1, 9.2, 9.3, 9.4, 9.5, 9.6, 9.7, 9.8, 9.9, 10, 10.1, 
                                                                                                    10.2, 10.3, 10.4, 10.5, 10.6, 10.7, 10.8, 10.9, 11, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8, 11.9, 12, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 12.8, 12.9, 13, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 13.8, 13.9, 14, 14.1, 14.2, 14.3, 14.4, 14.5, 14.6, 14.7, 14.8, 14.9, 15, 15.1, 15.2, 15.3, 15.4, 15.5, 15.6, 15.7, 15.8, 15.9, 16, 16.1, 16.2, 16.3, 16.4, 16.5, 16.6, 16.7, 16.8, 16.9, 17, 17.1, 17.2, 17.3, 17.4, 17.5, 17.6, 17.7, 17.8, 17.9, 18, 18.1, 18.2, 18.3, 
                                                                                                    Calls: estimateUCVM
                                                                                                    Execution halted
                                                                                                    ???  checking for unstated dependencies in vignettes
                                                                                                    W  checking package vignettes in 'inst/doc'
                                                                                                    Package vignette without corresponding PDF/HTML:
                                                                                                      'smoove.Rnw'
                                                                                                    
                                                                                                    W  checking re-building of vignette outputs
                                                                                                    Error in re-building vignettes:
                                                                                                      ...
                                                                                                    Warning in remind_sweave(if (in.file) input, sweave_lines) :
                                                                                                      It seems you are using the Sweave-specific syntax in line(s) 607; you may need Sweave2knitr("smoove.Rnw") to convert it to knitr
                                                                                                    Loading required package: smoove
                                                                                                    Loading required package: magrittr
                                                                                                    Loading required package: scales
                                                                                                    Warning in (if (out_format(c("latex", "sweave", "listings"))) sanitize_fn else paste0)(path,  :
                                                                                                                                                                                             dots in figure paths replaced with _ ("figure/ucvm_exact")
                                                                                                                                                                                           Warning in (if (out_format(c("latex", "sweave", "listings"))) sanitize_fn else paste0)(path,  :
                                                                                                                                                                                                                                                                                    dots in figure paths replaced with _ ("figure/K1_sim")
                                                                                                                                                                                                                                                                                  Warning: running command '"C:\PROGRA~1\MIKTEX~1.9\miktex\bin\x64\texify.exe" --quiet --pdf "smoove.tex" --max-iterations=20 -I "C:/PROGRA~1/R/R-34~1.1/share/texmf/tex/latex" -I "C:/PROGRA~1/R/R-34~1.1/share/texmf/bibtex/bst"' had status 1
                                                                                                                                                                                                                                                                                  Error: running 'texi2dvi' on 'smoove.tex' failed
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  LaTeX errors:
                                                                                                                                                                                                                                                                                    n_test/smoove/vignettes/smoove.tex:253: LaTeX Error: \begin{document} ended by 
                                                                                                                                                                                                                                                                                  \end{kframe}.
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  See the LaTeX manual or LaTeX Companion for explanation.
                                                                                                                                                                                                                                                                                  Type  H <return>  for immediate help
                                                                                                                                                                                                                                                                                  n_test/smoove/vignettes/smoove.tex:254: LaTeX Error: \begin{document} ended by 
                                                                                                                                                                                                                                                                                  \end{knitrout}.
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  See the LaTeX manual or LaTeX Companion for explanation.
                                                                                                                                                                                                                                                                                  Type  H <return>  for immediate help
                                                                                                                                                                                                                                                                                  Execution halted
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  -- 2 errors ??? | 8 warnings ??? | 2 notes ???
                                                                                                                                                                                                                                                                                  See
                                                                                                                                                                                                                                                                                  'C:/Users/farid/AppData/Local/Temp/RtmpuYJUHv/file38bc3fa051f1/smoove.Rcheck/00check.log'
                                                                                                                                                                                                                                                                                  for details.
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  Warning message:
                                                                                                                                                                                                                                                                                    roxygen2 requires Encoding: UTF-8 
                                                                                                                                                                                                                                                                                  > 