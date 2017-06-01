with hour as (
	SELECT
		station_id
		, hour(last_updated) AS hour
		, last_updated
		, bikes
		, docks
	FROM bikeshare_deduped
	WHERE station_id = 31000
	GROUP BY 1,2
)

SELECT 
	station_id
	, 