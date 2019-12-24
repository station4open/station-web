<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/home'>
	<html>
		<head>
			<title>Station</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<p>
				The settings of your account can <a href='/account'>be changed here</a>.
			</p>
			<xsl:if test='session/role="SysOp"'>
				<p>
					System operator can manage <a href='/sysop/account'>accounts</a> and <a href='/sysop/subjects'>lessons</a> of
					this site.
				</p>
			</xsl:if>
			<h1>Subjects</h1>
			<xsl:for-each select='subjects/subject'>
				<h2>
					<a>
						<xsl:attribute name='href'>learn/subject/<xsl:value-of select='identifier'/></xsl:attribute>
						<xsl:value-of select='title'/>
					</a>
				</h2>
				<p><xsl:value-of select='description'/></p>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
