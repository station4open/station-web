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
			<link rel="stylesheet" type="text/css" href="base.css"/>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<xsl:for-each select='course'>
				<form action='subject.xml' method='POST'>
					<input type='hidden' name='subject'>
						<xsl:attribute name='value'><xsl:value-of select='subject'/></xsl:attribute>
					</input>
					<input type='submit' value='Back'/>
				</form>
				<section>
					<h1>Course</h1>
					<form method='POST'>
						<div class='flex'>
							<input type='text' name='subject' readonly=''>
								<xsl:attribute name='value'><xsl:value-of select='subject'/></xsl:attribute>
							</input>
						</div>
						<div class='flex'>
							<input type='text' name='course' readonly=''>
								<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
							</input>
						</div>
						<label>
							Title
							<div class='flex'>
								<input type='text' name='title'>
									<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
								</input>
							</div>
						</label>
						<label>
							Description
							<div class='flex'>
								<textarea name='description'><xsl:value-of select='description'/></textarea>
							</div>
						</label>
						<label>Delete<input type='checkbox' name='delete'/></label>
						<br/>
						<input type='submit' value='Save'/>
					</form>
				</section>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
