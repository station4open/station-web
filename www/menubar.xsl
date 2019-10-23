<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template name='menubar'>
	<form action='/bin/logout' method='POST'>
		<a href='/home.xml'>Home</a>
		|
		<button type='submit'>Log out</button>
	</form>
</xsl:template>
</xsl:stylesheet>
