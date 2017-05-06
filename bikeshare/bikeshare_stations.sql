DROP TABLE IF EXISTS bikeshare.stations;
CREATE TABLE bikeshare.stations (
	row_names INT,
	id INT,
	name VARCHAR(255),
	terminalName INT ,
	lastCommWithServer DATETIME,
	lat FLOAT,
	long FLOAT,
	installed BOOL,
	locked BOOL,
	installDate INT,
	temporary BOOL,
	public BOOL,
	nbBikes INT,
	nbEmptyDocks INT,
	latestUpdateTime DATETIME,
	popup VARCHAR,
	date_pulled DATETIME
);

INSERT INTO bikeshare.stations
SELECT * FROM bikeshare_stations;