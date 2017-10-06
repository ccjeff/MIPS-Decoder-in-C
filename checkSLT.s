# Short test case for your project.
#
# Note that this is by no means a comprehensive test!
#

		.text
		addiu	$t0,$0,3
		addiu	$a1,$0,4
		slt	$a0,$t0,$a1
		beq	$a0,$0, Done
		slt	$a0,$a1,$t0
		beq	$a0,$0, Done
		addiu	$t0,$0,3
		addiu	$a1,$0,4
		addiu	$t0,$0,3
		addiu	$a1,$0,4
		addiu	$t0,$0,3
		addiu	$a1,$0,4
Done:	
		addi	$t0, $a0, 5