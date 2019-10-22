<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform' version='1.0'>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/account'>
	<html xmlns='http://www.w3.org/1999/xhtml'>
		<head>
			<title>Account</title>
		</head>
		<body>
			<form method='POST'>
				<label>
					New password:
					<input type='text' name='password'/>
				</label>
				<button type='submit'>Change</button>
			</form>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
