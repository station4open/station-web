<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/'>
	<html>
		<head>
			<title>SysOp: Course</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<xsl:for-each select='course'>
				<form action='subject.xml' method='POST'>
					<input type='hidden' name='subject'>
						<xsl:attribute name='value'><xsl:value-of select='subject'/></xsl:attribute>
					</input>
					<button type='submit'>Back</button>
				</form>
				<section>
					<h1><xsl:value-of select='subject'/>: <xsl:value-of select='title'/></h1>
					<pre><xsl:value-of select='description'/></pre>
				</section>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
