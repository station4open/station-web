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
			<title>SysOp: Subject</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<section>
				<h1>Create new subject</h1>
				<p>
					<form method='POST'>
						<label>
							Title
							<br/>
							<input type='text' name='title'/>
						</label>
						<br/>
						<label>
							Description
							<br/>
							<textarea name='description'/>
						</label>
						<br/>
						<button type='submit'>Create</button>
					</form>
				</p>
			</section>
			<section>
				<h1>Modify existing subjects</h1>
				<xsl:for-each select='item'>
					<p>
						<form method='POST'>
							<input type='text' name='subject' readonly=''>
								<xsl:attribute name='value'><xsl:value-of select='@title'/></xsl:attribute>
							</input>
							<br/>
							<label>
								Title
								<br/>
								<input type='text' name='title'>
									<xsl:attribute name='value'><xsl:value-of select='@title'/></xsl:attribute>
								</input>
							</label>
							<br/>
							<label>
								Description
								<br/>
								<textarea name='description'><xsl:value-of select='.'/></textarea>
							</label>
							<br/>
							<label>Delete<input type='checkbox' name='delete'/></label>
							<br/>
							<button type='submit'>Modify</button>
						</form>
					</p>
				</xsl:for-each>
			</section>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
