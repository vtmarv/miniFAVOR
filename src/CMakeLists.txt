add_library(minifav
  Calc_cpi.f90
  Calc_K.f90
  Calc_RTndt.f90
  Commons.f90
  I_O.f90
  random_samples_m.f90
  random_samples_s.f90
)
target_link_libraries(minifav
  PRIVATE sourcery
)
add_subdirectory(git-submodules)
