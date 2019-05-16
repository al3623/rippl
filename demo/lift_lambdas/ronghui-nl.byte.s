	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbx
Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
Ltmp1:
	.cfi_def_cfa_offset 32
Ltmp2:
	.cfi_offset %rbx, -16
	callq	_initNativeThunks
	leaq	_$$d_init_thunk(%rip), %rdi
	leaq	_$eval_d(%rip), %rsi
	movl	$2, %edx
	xorl	%ecx, %ecx
	callq	_init_thunk
	leaq	_$$b_init_thunk(%rip), %rdi
	leaq	_$eval_b(%rip), %rsi
	movl	$2, %edx
	xorl	%ecx, %ecx
	callq	_init_thunk
	leaq	_$$e_init_thunk(%rip), %rdi
	leaq	_$eval_e(%rip), %rsi
	movl	$3, %edx
	xorl	%ecx, %ecx
	callq	_init_thunk
	leaq	_$$a_init_thunk(%rip), %rbx
	leaq	_$eval_a(%rip), %rsi
	movl	$2, %edx
	xorl	%ecx, %ecx
	movq	%rbx, %rdi
	callq	_init_thunk
	movl	$0, 12(%rsp)
	movl	$61, %edi
	callq	_makeInt
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%rax, %rbx
	movl	$5, %edi
	callq	_makeInt
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	_invoke
	leaq	12(%rsp), %rsi
	xorl	%edx, %edx
	movq	%rbx, %rdi
	callq	_printAnyThunk
	leaq	L_fmt(%rip), %rdi
	movl	$10, %esi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc

	.globl	_$eval_d
	.p2align	4, 0x90
_$eval_d:                               ## @"$eval_d"
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$40, %rsp
Ltmp3:
	.cfi_def_cfa_offset 48
	movq	%rdi, 16(%rsp)
	movq	16(%rdi), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 32(%rsp)
	movq	8(%rsp), %rdi
	callq	_d
	movq	%rax, 24(%rsp)
	addq	$40, %rsp
	retq
	.cfi_endproc

	.globl	_$eval_b
	.p2align	4, 0x90
_$eval_b:                               ## @"$eval_b"
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$40, %rsp
Ltmp4:
	.cfi_def_cfa_offset 48
	movq	%rdi, 16(%rsp)
	movq	16(%rdi), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 32(%rsp)
	movq	8(%rsp), %rdi
	callq	_b
	movq	%rax, 24(%rsp)
	addq	$40, %rsp
	retq
	.cfi_endproc

	.globl	_$eval_e
	.p2align	4, 0x90
_$eval_e:                               ## @"$eval_e"
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$40, %rsp
Ltmp5:
	.cfi_def_cfa_offset 48
	movq	%rdi, (%rsp)
	movq	16(%rdi), %rax
	movq	(%rax), %rax
	movq	%rax, 16(%rsp)
	movq	(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	(%rsp), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rdx
	movq	%rdx, 32(%rsp)
	movq	8(%rsp), %rsi
	movq	16(%rsp), %rdi
	callq	_e
	movq	%rax, 24(%rsp)
	addq	$40, %rsp
	retq
	.cfi_endproc

	.globl	_$eval_a
	.p2align	4, 0x90
_$eval_a:                               ## @"$eval_a"
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$40, %rsp
Ltmp6:
	.cfi_def_cfa_offset 48
	movq	%rdi, 16(%rsp)
	movq	16(%rdi), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 32(%rsp)
	movq	8(%rsp), %rdi
	callq	_a
	movq	%rax, 24(%rsp)
	addq	$40, %rsp
	retq
	.cfi_endproc

	.globl	_d
	.p2align	4, 0x90
_d:                                     ## @d
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbx
Ltmp7:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
Ltmp8:
	.cfi_def_cfa_offset 48
Ltmp9:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rbx, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	_add_init_thunk(%rip), %rdi
	callq	_apply
	movq	%rbx, (%rsp)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_apply
	movq	%rax, %rdi
	callq	_invoke
	addq	$32, %rsp
	popq	%rbx
	retq
	.cfi_endproc

	.globl	_b
	.p2align	4, 0x90
_b:                                     ## @b
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%r14
Ltmp10:
	.cfi_def_cfa_offset 16
	pushq	%rbx
Ltmp11:
	.cfi_def_cfa_offset 24
	subq	$40, %rsp
Ltmp12:
	.cfi_def_cfa_offset 64
Ltmp13:
	.cfi_offset %rbx, -24
Ltmp14:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %rax
	movq	%rax, 32(%rsp)
	movq	%rbx, 24(%rsp)
	movq	%rax, 16(%rsp)
	leaq	_$$d_init_thunk(%rip), %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%rax, %r14
	movq	%rbx, 8(%rsp)
	leaq	_add_init_thunk(%rip), %rdi
	movq	%rbx, %rsi
	callq	_apply
	movq	%rax, %rbx
	movl	$1, %edi
	callq	_makeInt
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%rax, %rdi
	callq	_invoke
	addq	$40, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc

	.globl	_e
	.p2align	4, 0x90
_e:                                     ## @e
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%r14
Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%rbx
Ltmp16:
	.cfi_def_cfa_offset 24
	subq	$56, %rsp
Ltmp17:
	.cfi_def_cfa_offset 80
Ltmp18:
	.cfi_offset %rbx, -24
Ltmp19:
	.cfi_offset %r14, -16
	movq	%rdx, %rbx
	movq	%rdi, 48(%rsp)
	movq	%rsi, 40(%rsp)
	movq	%rbx, 32(%rsp)
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	callq	_apply
	movq	%rax, %r14
	movq	%rbx, 8(%rsp)
	leaq	_add_init_thunk(%rip), %rdi
	movq	%rbx, %rsi
	callq	_apply
	movq	%rax, %rbx
	movl	$1, %edi
	callq	_makeInt
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%rax, %rdi
	callq	_invoke
	addq	$56, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc

	.globl	_a
	.p2align	4, 0x90
_a:                                     ## @a
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%r15
Ltmp20:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp21:
	.cfi_def_cfa_offset 24
	pushq	%rbx
Ltmp22:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
Ltmp23:
	.cfi_def_cfa_offset 64
Ltmp24:
	.cfi_offset %rbx, -32
Ltmp25:
	.cfi_offset %r14, -24
Ltmp26:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, 24(%rsp)
	movq	%r14, 16(%rsp)
	leaq	_$$e_init_thunk(%rip), %rdi
	leaq	_$$b_init_thunk(%rip), %rsi
	callq	_apply
	movq	%rbx, 8(%rsp)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_apply
	movq	%rax, %r15
	movq	%r14, (%rsp)
	leaq	_add_init_thunk(%rip), %rdi
	movq	%r14, %rsi
	callq	_apply
	movq	%rax, %rbx
	movl	$1, %edi
	callq	_makeInt
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_apply
	movq	%rax, %rdi
	callq	_invoke
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%c"

L_fmt_int:                              ## @fmt_int
	.asciz	"%d"

L_fmt_float:                            ## @fmt_float
	.asciz	"%f"

	.globl	_add_init_thunk         ## @add_init_thunk
.zerofill __DATA,__common,_add_init_thunk,40,5
	.globl	_sub_init_thunk         ## @sub_init_thunk
.zerofill __DATA,__common,_sub_init_thunk,40,5
	.globl	_mult_init_thunk        ## @mult_init_thunk
.zerofill __DATA,__common,_mult_init_thunk,40,5
	.globl	_divi_init_thunk        ## @divi_init_thunk
.zerofill __DATA,__common,_divi_init_thunk,40,5
	.globl	_mod_init_thunk         ## @mod_init_thunk
.zerofill __DATA,__common,_mod_init_thunk,40,5
	.globl	_powe_init_thunk        ## @powe_init_thunk
.zerofill __DATA,__common,_powe_init_thunk,40,5
	.globl	_eq_init_thunk          ## @eq_init_thunk
.zerofill __DATA,__common,_eq_init_thunk,40,5
	.globl	_neq_init_thunk         ## @neq_init_thunk
.zerofill __DATA,__common,_neq_init_thunk,40,5
	.globl	_geq_init_thunk         ## @geq_init_thunk
.zerofill __DATA,__common,_geq_init_thunk,40,5
	.globl	_leq_init_thunk         ## @leq_init_thunk
.zerofill __DATA,__common,_leq_init_thunk,40,5
	.globl	_less_init_thunk        ## @less_init_thunk
.zerofill __DATA,__common,_less_init_thunk,40,5
	.globl	_greater_init_thunk     ## @greater_init_thunk
.zerofill __DATA,__common,_greater_init_thunk,40,5
	.globl	_neg_init_thunk         ## @neg_init_thunk
.zerofill __DATA,__common,_neg_init_thunk,40,5
	.globl	_addf_init_thunk        ## @addf_init_thunk
.zerofill __DATA,__common,_addf_init_thunk,40,5
	.globl	_subf_init_thunk        ## @subf_init_thunk
.zerofill __DATA,__common,_subf_init_thunk,40,5
	.globl	_multf_init_thunk       ## @multf_init_thunk
.zerofill __DATA,__common,_multf_init_thunk,40,5
	.globl	_divf_init_thunk        ## @divf_init_thunk
.zerofill __DATA,__common,_divf_init_thunk,40,5
	.globl	_powef_init_thunk       ## @powef_init_thunk
.zerofill __DATA,__common,_powef_init_thunk,40,5
	.globl	_eqf_init_thunk         ## @eqf_init_thunk
.zerofill __DATA,__common,_eqf_init_thunk,40,5
	.globl	_neqf_init_thunk        ## @neqf_init_thunk
.zerofill __DATA,__common,_neqf_init_thunk,40,5
	.globl	_geqf_init_thunk        ## @geqf_init_thunk
.zerofill __DATA,__common,_geqf_init_thunk,40,5
	.globl	_leqf_init_thunk        ## @leqf_init_thunk
.zerofill __DATA,__common,_leqf_init_thunk,40,5
	.globl	_lessf_init_thunk       ## @lessf_init_thunk
.zerofill __DATA,__common,_lessf_init_thunk,40,5
	.globl	_greaterf_init_thunk    ## @greaterf_init_thunk
.zerofill __DATA,__common,_greaterf_init_thunk,40,5
	.globl	_negf_init_thunk        ## @negf_init_thunk
.zerofill __DATA,__common,_negf_init_thunk,40,5
	.globl	_andb_init_thunk        ## @andb_init_thunk
.zerofill __DATA,__common,_andb_init_thunk,40,5
	.globl	_orb_init_thunk         ## @orb_init_thunk
.zerofill __DATA,__common,_orb_init_thunk,40,5
	.globl	_notb_init_thunk        ## @notb_init_thunk
.zerofill __DATA,__common,_notb_init_thunk,40,5
	.globl	_cons_init_thunk        ## @cons_init_thunk
.zerofill __DATA,__common,_cons_init_thunk,40,5
	.globl	_cat_init_thunk         ## @cat_init_thunk
.zerofill __DATA,__common,_cat_init_thunk,40,5
	.globl	_length_init_thunk      ## @length_init_thunk
.zerofill __DATA,__common,_length_init_thunk,40,5
	.globl	_head_init_thunk        ## @head_init_thunk
.zerofill __DATA,__common,_head_init_thunk,40,5
	.globl	_tail_init_thunk        ## @tail_init_thunk
.zerofill __DATA,__common,_tail_init_thunk,40,5
	.globl	_is_none_init_thunk     ## @is_none_init_thunk
.zerofill __DATA,__common,_is_none_init_thunk,40,5
	.globl	_from_just_init_thunk   ## @from_just_init_thunk
.zerofill __DATA,__common,_from_just_init_thunk,40,5
	.globl	_first_init_thunk       ## @first_init_thunk
.zerofill __DATA,__common,_first_init_thunk,40,5
	.globl	_second_init_thunk      ## @second_init_thunk
.zerofill __DATA,__common,_second_init_thunk,40,5
	.globl	_ite_init_thunk         ## @ite_init_thunk
.zerofill __DATA,__common,_ite_init_thunk,40,5
	.globl	_int_to_float_init_thunk ## @int_to_float_init_thunk
.zerofill __DATA,__common,_int_to_float_init_thunk,40,5
	.globl	_$$d_init_thunk         ## @"$$d_init_thunk"
.zerofill __DATA,__common,_$$d_init_thunk,40,4
	.globl	_$$b_init_thunk         ## @"$$b_init_thunk"
.zerofill __DATA,__common,_$$b_init_thunk,40,4
	.globl	_$$e_init_thunk         ## @"$$e_init_thunk"
.zerofill __DATA,__common,_$$e_init_thunk,40,4
	.globl	_$$a_init_thunk         ## @"$$a_init_thunk"
.zerofill __DATA,__common,_$$a_init_thunk,40,4

.subsections_via_symbols
