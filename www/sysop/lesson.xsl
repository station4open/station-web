<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/lesson'>
	<html>
		<head>
			<title>SysOp: Lesson</title>
			<link rel="stylesheet" type="text/css" href="../base.css"/>
			<link rel="stylesheet" type="text/css" href="/base.css"/>
			<script type='application/ecmascript' src='../delete_disable.js' async=''/>
			<style>
				p {
					border-style: double;
					padding: 1ex;
				}
				p > form {
					display: inline;
					margin-right: 1ex;
				}
			</style>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<a>
				<xsl:attribute name='href'>../course/<xsl:value-of select='course'/></xsl:attribute>
				Back
			</a>
			<section>
				<h1>Lesson</h1>
				<form method='POST'>
					<xsl:attribute name='action'><xsl:value-of select='identifier'/></xsl:attribute>
					<h2><xsl:value-of select='number'/>. <xsl:value-of select='title'/></h2>
					<input type='hidden' name='course'>
						<xsl:attribute name='value'><xsl:value-of select='course'/></xsl:attribute>
					</input>
					<input type='hidden' name='number'>
						<xsl:attribute name='value'><xsl:value-of select='number'/></xsl:attribute>
					</input>
					<label>
						Title
						<div class='flex'>
							<input type='text' name='title'>
								<xsl:attribute name='value'><xsl:value-of select='title'/></xsl:attribute>
							</input>
						</div>
					</label>
					<label>
						Content
						<div class='flex'>
							<textarea name='content'><xsl:value-of select='content'/></textarea>
						</div>
					</label>
					<label>Delete<input type='checkbox' name='delete'/></label>
					<br/>
					<button type='submit'>Modify</button>
				</form>
			</section>
			<section>
				<h1>Embed</h1>
				<form method='POST' enctype='multipart/form-data'>
					<xsl:attribute name='action'>../embed/new/<xsl:value-of select='identifier'/></xsl:attribute>
					<fieldset>
						<legend>Upload File</legend>
						<label>
							Title:
							<input type='text' name='title'/>
						</label>
						<label>
							PNG or JPEG file:
							<input type='file' name='file' required='' accept='image/png, image/jpeg'/>
						</label>
						<button type='submit'>Submit</button>
					</fieldset>
				</form>
				<form method='POST'>
					<xsl:attribute name='action'>../embed/new/<xsl:value-of select='identifier'/></xsl:attribute>
					<fieldset>
						<legend>YouTube video</legend>
						<label>
							Title:
							<input type='text' name='title'/>
						</label>
						<label>
							Link:
							<input type='text' name='youtube' required=''/>
						</label>
						<button type='submit'>Submit</button>
					</fieldset>
				</form>
			</section>
			<section>
				<h1>Modify Embed</h1>
					<xsl:for-each select='embeds/embed'>
					<p>
						<form method='POST'>
							<xsl:attribute name='action'>
								../embed/<xsl:value-of select='identifier'/>
							</xsl:attribute>
							<input type='hidden' name='lesson'>
								<xsl:attribute name='value'>
									<xsl:value-of select='../../identifier'/>
								</xsl:attribute>
							</input>
							<input type='hidden' name='delete'/>
							<button type='submit'>Delete</button>
						</form>
						<xsl:if test='number>1'>
							<form method='POST'>
								<xsl:attribute name='action'>
									../embed/<xsl:value-of select='identifier'/>
								</xsl:attribute>
								<input type='hidden' name='lesson'>
									<xsl:attribute name='value'>
										<xsl:value-of select='../../identifier'/>
									</xsl:attribute>
								</input>
								<input type='hidden' name='exchange'>
									<xsl:attribute name='value'>
										<xsl:value-of select='preceding-sibling::*[1]/identifier'/>
									</xsl:attribute>
								</input>
								<button type='submit'>↑</button>
							</form>
						</xsl:if>
						<xsl:if test='count(../embed)>number'>
							<form method='POST'>
								<xsl:attribute name='action'>
									../embed/<xsl:value-of select='identifier'/>
								</xsl:attribute>
								<input type='hidden' name='lesson'>
									<xsl:attribute name='value'>
										<xsl:value-of select='../../identifier'/>
									</xsl:attribute>
								</input>
								<input type='hidden' name='exchange'>
									<xsl:attribute name='value'>
										<xsl:value-of select='following-sibling::*[1]/identifier'/>
									</xsl:attribute>
								</input>
								<button type='submit'>↓</button>
							</form>
						</xsl:if>
						<form method='POST'>
							<xsl:attribute name='action'>
								../embed/<xsl:value-of select='identifier'/>
							</xsl:attribute>
							<input type='hidden' name='lesson'>
								<xsl:attribute name='value'>
									<xsl:value-of select='../../identifier'/>
								</xsl:attribute>
							</input>
							<input type='text' name='title'>
								<xsl:attribute name='value'>
									<xsl:value-of select='title'/>
								</xsl:attribute>
							</input>
							<button type='submit'>Change</button>
						</form>
						<br/>
						<xsl:choose>
							<xsl:when test='kind=1 or kind=2'>
								<td colspan='4'>
									<img>
										<xsl:attribute name='src'>/learn/embed/<xsl:value-of select='identifier'/></xsl:attribute>
									</img>
								</td>
							</xsl:when>
							<xsl:when test='kind=101'>
								<td colspan='4'>
									<iframe
										width="560" height="315"
										title="YouTube video player"
										frameborder="0"
										allow="accelerometer; encrypted-media; gyroscope; picture-in-picture; fullscreen"
										allowfullscreen=''
									>
										<xsl:attribute name='src'>https://www.youtube.com/embed/<xsl:value-of select='value'/></xsl:attribute>
									</iframe>
								</td>
							</xsl:when>
						</xsl:choose>
					</p>
				</xsl:for-each>
			</section>
			<section>
				<h1>Create question</h1>
				<form method='POST'>
					<xsl:attribute name='action'>../question/new/<xsl:value-of select='identifier'/></xsl:attribute>
					<label>
						Question
						<div class='flex'>
							<textarea name='text'/>
						</div>
					</label>
					<button type='submit'>Create</button>
				</form>
			</section>
			<section>
				<h1>Modify question</h1>
				<table>
					<xsl:for-each select='questions/question'>
						<tbody>
							<tr>
								<td>
									<a>
										<xsl:attribute name='href'>../question/<xsl:value-of select='identifier'/></xsl:attribute>
										<xsl:value-of select='number'/>
									</a>.
								</td>
								<td>
									<xsl:if test='number>1'>
										<form method='POST'>
											<xsl:attribute name='action'>
												../question/exchange/<xsl:value-of select='../../identifier'/>
											</xsl:attribute>
											<input type='hidden' name='0'>
												<xsl:attribute name='value'>
													<xsl:value-of select='identifier'/>
												</xsl:attribute>
											</input>
											<input type='hidden' name='1'>
												<xsl:attribute name='value'>
													<xsl:value-of select='preceding-sibling::*[1]/identifier'/>
												</xsl:attribute>
											</input>
											<button type='submit'>↑</button>
										</form>
									</xsl:if>
								</td>
								<td>
									<xsl:if test='count(../question)>number'>
										<form method='POST'>
											<xsl:attribute name='action'>
												../question/exchange/<xsl:value-of select='../../identifier'/>
											</xsl:attribute>
											<input type='hidden' name='0'>
												<xsl:attribute name='value'>
													<xsl:value-of select='identifier'/>
												</xsl:attribute>
											</input>
											<input type='hidden' name='1'>
												<xsl:attribute name='value'>
													<xsl:value-of select='following-sibling::*[1]/identifier'/>
												</xsl:attribute>
											</input>
											<button type='submit'>↓</button>
										</form>
									</xsl:if>
								</td>
								<td>
									<xsl:value-of select='text'/>
								</td>
							</tr>
						</tbody>
					</xsl:for-each>
				</table>
			</section>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
