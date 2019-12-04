<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/lesson'>
	<html>
		<head>
			<title>Station - Lesson</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<h1><xsl:value-of select='number'/>. <xsl:value-of select='title'/></h1>
			<p><xsl:value-of select='content'/></p>
			<h1>Question</h1>
			<xsl:for-each select='questions/question'>
				<ul>
					<li><xsl:value-of select='text'/></li>
				</ul>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
