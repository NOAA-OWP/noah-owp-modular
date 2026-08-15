# Per-compiler, per-config Fortran flag matrix for noah-owp-modular.
#
# Applies per-config flags via generator expressions so a single build tree
# can switch configurations and multi-config generators pick the right set.
# Flags are grouped by purpose so nothing has to be repeated per config;
# _language covers the preprocessor, line length and floating-point options
# that every configuration needs.

function(noahowp_apply_fortran_flags target)
    set(_id "${CMAKE_Fortran_COMPILER_ID}")
    set(_optimize -O2)

    if(_id MATCHES "GNU")
        set(_language -ffree-line-length-none -frounding-math -fno-fast-math -cpp)
        set(_debuginfo -g -fbacktrace)
        set(_checks -Wall -fcheck=all)
    elseif(_id MATCHES "Intel|IntelLLVM")
        set(_language -fp-model=strict -fpp)
        set(_debuginfo -g -traceback)
        set(_checks -check all -warn all)
    elseif(_id MATCHES "NVIDIA|NVHPC|PGI")
        set(_language -Kieee -Mbackslash -Mpreprocess)
        set(_debuginfo -g -traceback)
        set(_checks -Mbounds -Mchkptr)
    else()
        message(WARNING
            "noahowp: unrecognized Fortran compiler '${_id}'; no flags applied")
        return()
    endif()

    target_compile_options(${target} PRIVATE
        ${_language}
        $<$<CONFIG:Debug>:${_debuginfo};${_checks}>
        $<$<CONFIG:Release>:${_optimize}>
        $<$<CONFIG:RelWithDebInfo>:${_optimize};${_debuginfo}>)
endfunction()
