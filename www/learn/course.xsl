<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/course'>
	<html>
		<head>
			<title>Station - Course</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<h1><xsl:value-of select='title'/></h1>
			<p><xsl:value-of select='description'/></p>
			<h1>Lessons</h1>
			<xsl:for-each select='lessons/lesson'>
				<p>
					<xsl:value-of select='number'/>.
					<a>
						<xsl:attribute name='href'>../lesson/<xsl:value-of select='identifier'/></xsl:attribute>
						<xsl:value-of select='title'/>
					</a>
				</p>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
