# Short test case for your project.
#
# Note that this is by no means a comprehensive test!
#

		.text
		addiu	$a0,$0,3
		lui	$t1, 0x0040
		ori	$t1, $t1, 0x1020 
		sw	$a0, 0($t1)
		lw	$a1, 0($t1)
		addiu	$a1,$0,2
		jal	Mystery
		addi	$0,$0,0 #unsupported instruction, terminate


Mystery:
		addiu	$v0,$0,0
Loop:
		beq	$a0,$0,Done
		addu	$v0,$v0,$a1
		addiu	$a0,$a0,-1
		j Loop
Done:	
		jr		$ra