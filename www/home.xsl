<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform' version='1.0'>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/home'>
	<html xmlns='http://www.w3.org/1999/xhtml'>
		<head>
			<title>Station</title>
		</head>
		<body>
			<p>
				You have logged in.
			</p>
			<p>
				Your log-in password can be changed <a href='/account.xml'>here</a>.
			</p>
			<form method='POST' action='/bin/logout'>
				<button type='submit'>Log out</button>
			</form>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
