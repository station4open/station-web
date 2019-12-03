<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/subject'>
	<html>
		<head>
			<title>Station - <xsl:value-of select='title'/></title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<ul>
				<li>identifier: <xsl:value-of select='identifier'/></li>
				<li>title: <xsl:value-of select='title'/></li>
				<li>description: <xsl:value-of select='description'/></li>
			</ul>
			<h1>Courses</h1>
			<xsl:for-each select='courses/course'>
				<ul>
					<li>identifier: <xsl:value-of select='identifier'/></li>
					<li>title: <xsl:value-of select='title'/></li>
					<li>description: <xsl:value-of select='description'/></li>
				</ul>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
