# Per-compiler, per-config Fortran flag matrix for noah-owp-modular.
#
# Applies Debug/Release flags via generator expressions so a single build tree
# can switch configurations and multi-config generators pick the right set.

function(noahowp_apply_fortran_flags target)
    set(_id "${CMAKE_Fortran_COMPILER_ID}")

    if(_id MATCHES "GNU")
        set(_debug
            -g -fbacktrace -Wall -fcheck=all
            -ffree-line-length-none -frounding-math -fno-fast-math -cpp)
        set(_release
            -O2 -ffree-line-length-none -frounding-math -fno-fast-math -cpp)
    elseif(_id MATCHES "Intel|IntelLLVM")
        set(_debug  -g -traceback -check all -warn all -fp-model=strict -fpp)
        set(_release -O2 -fp-model=strict -fpp)
    elseif(_id MATCHES "NVIDIA|NVHPC|PGI")
        set(_debug
            -g -traceback -Mbounds -Mchkptr -Kieee -Mbackslash -Mpreprocess)
        set(_release -O2 -Kieee -Mbackslash -Mpreprocess)
    else()
        message(WARNING
            "noahowp: unrecognized Fortran compiler '${_id}'; no flags applied")
        return()
    endif()

    target_compile_options(${target} PRIVATE
        $<$<CONFIG:Debug>:${_debug}>
        $<$<CONFIG:Release>:${_release}>
        $<$<CONFIG:RelWithDebInfo>:${_release}>)
endfunction()
