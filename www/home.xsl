<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/home'>
	<html>
		<head>
			<title>Station</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<p>
				Welcome <strong><xsl:value-of select='@name'/></strong>.
			</p>
			<p>
				The settings of your account can <a href='/account.xml'>be changed here</a>.
			</p>
			<xsl:if test='@role="SysOp"'>
				<p>
					SysOp can also manage <a href='/sysop/account.xml'>accounts</a>.
				</p>
			</xsl:if>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
