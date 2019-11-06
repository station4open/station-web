CREATE TABLE "USER"(
	"NAME" TEXT NOT NULL,
	"PASSWORD" TEXT NOT NULL,
	"ROLE" SMALLINT NOT NULL,
	PRIMARY KEY("NAME")
);

CREATE TABLE "SUBJECT"(
	"TITLE" TEXT NOT NULL,
	"DESCRIPTION" TEXT NOT NULL,
	PRIMARY KEY("TITLE")
);

CREATE TABLE "COURSE"(
	"SUBJECT" TEXT NOT NULL,
	"TITLE" TEXT NOT NULL,
	"DESCRIPTION" TEXT NOT NULL,
	PRIMARY KEY("SUBJECT", "TITLE"),
	FOREIGN KEY("SUBJECT") REFERENCES "SUBJECT"("TITLE")
		ON DELETE CASCADE
		ON UPDATE CASCADE
);
