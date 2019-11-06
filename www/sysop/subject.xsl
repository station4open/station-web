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
			<title>SysOp: Subject</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<xsl:for-each select='subjects'>
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
								<button type='submit'>Open</button>
								<input type='text' name='subject' readonly=''>
									<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
								</input>
							</form>
						</p>
						<p><xsl:value-of select='description'/></p>
					</xsl:for-each>
				</section>
			</xsl:for-each>
			<xsl:for-each select='subject'>
				<form method='GET'>
					<button type='submit'>Back</button>
				</form>
				<section>
					<h1>Subject</h1>
					<p>
						<form method='POST'>
							<input type='text' name='subject' readonly=''>
								<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
							</input>
							<br/>
							<label>
								Title
								<br/>
								<input type='text' name='title'>
									<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
								</input>
							</label>
							<br/>
							<label>
								Description
								<br/>
								<textarea name='description'><xsl:value-of select='description'/></textarea>
							</label>
							<br/>
							<label>Delete<input type='checkbox' name='delete'/></label>
							<br/>
							<button type='submit'>Save</button>
						</form>
					</p>
				</section>
				<section>
					<h1>Modify existing courses</h1>
					<xsl:for-each select='courses/course'>
						<p>
							<form action='course.xml' method='POST'>
								<button type='submit'>Open</button>
								<input type='hidden' name='subject'>
									<xsl:attribute name='value'><xsl:value-of select='../../title'/></xsl:attribute>
								</input>
								<input type='text' name='course' readonly=''>
									<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
								</input>
								<br/>
								<xsl:value-of select='description'/>
							</form>
						</p>
					</xsl:for-each>
				</section>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
