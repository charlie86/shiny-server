SELECT 
	created_at,
	candidate,
	sum(positive) AS pct_pos
FROM twitts
GROUP BY 1,2