<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/account' xmlns='http://www.w3.org/1999/xhtml'>
	<html>
		<head>
			<title>SysOp: Account</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<h2>Create new account</h2>
			<form method='POST'>
				<label>
					Name
					<input type='text' name='name'/>
				</label>
				<label>
					Role
					<select name='role'>
						<option value='User'>User</option>
						<option value='SysOp'>SysOp</option>
					</select>
				</label>
				<label>
					New Password
					<input type='text' name='password'/>
				</label>
				<button type='submit'>Save</button>
			</form>
			<h2>Modify existing accounts</h2>
			<xsl:for-each select='user'>
				<form method='POST'>
					<label>
						Account
						<input type='text' name='user' readonly=''>
							<xsl:attribute name='value'>
								<xsl:value-of select='.'/>
							</xsl:attribute>
						</input>
					</label>
					<label>
						Name
						<input type='text' name='name'>
							<xsl:attribute name='value'><xsl:value-of select='.'/></xsl:attribute>
						</input>
					</label>
					<label>
						Role
						<select name='role'>
							<option value='User'>
								<xsl:if test='@role="User"'>
									<xsl:attribute name='selected'/>
								</xsl:if>
								User
							</option>
							<option value='SysOp'>
								<xsl:if test='@role="SysOp"'>
									<xsl:attribute name='selected'/>
								</xsl:if>
								SysOp
							</option>
						</select>
					</label>
					<label>
						New Password
						<input type='text' name='password'/>
					</label>
					<button type='submit'>Save</button>
				</form>
			</xsl:for-each>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
