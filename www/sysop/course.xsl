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
			<title>SysOp: Course</title>
			<link rel="stylesheet" type="text/css" href="../base.css"/>
			<script type='application/ecmascript' src='../../../delete_disable.js' async=''/>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<a>
				<xsl:attribute name='href'>../subject/<xsl:value-of select='subject'/></xsl:attribute>
				Back
			</a>
			<section>
				<h1>Course</h1>
				<h2><xsl:value-of select='title'/></h2>
				<form method='POST'>
					<div class='flex'>
						<input type='hidden' name='subject'>
							<xsl:attribute name='value'><xsl:value-of select='subject'/></xsl:attribute>
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
					<button type='submit'>Modify</button>
				</form>
			</section>
			<section>
				<xsl:for-each select='lessons'>
					<h1>Lessons</h1>
					<xsl:for-each select='lesson'>
						<p><xsl:value-of select='number'/>: <xsl:value-of select='title'/></p>
					</xsl:for-each>
				</xsl:for-each>
			</section>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
