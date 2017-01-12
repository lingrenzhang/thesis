
public class User {
	// user features
	private String keyword;
	private String description;
	private String name;
	private String screenName;
	private int followerCount;
	private int friendCount;
	private int statusCount;
	
	// match quality
	private boolean uniqueAmongRawResults;
	private boolean uniqueAmongFilteredResults;
	private int matchScore;
	
	public User (	String keyword, String description, String name, String screenName, 
					int followerCount, int friendCount, int statusCount,
					boolean uniqueAmongRawResults, boolean uniqueAmongFilteredResults, int matchScore	) {
		this.keyword = keyword;
		this.description = description;
		this.name = name;
		this.screenName = screenName;
		this.followerCount = followerCount;
		this.friendCount = friendCount;
		this.statusCount = statusCount;
		this.uniqueAmongRawResults = uniqueAmongRawResults;
		this.uniqueAmongFilteredResults = uniqueAmongFilteredResults;
		this.matchScore = matchScore;
	}
	
	public String getKeyword() {
		return keyword;
	}
	
	public String getDescription() {
		return description;
	}
	
	public String getName() {
		return name;
	}

	public String getScreenName() {
		return screenName;
	}
	

	public int getFollowerCount() {
		return followerCount;
	}

	public int getFriendCount() {
		return friendCount;
	}
	
	public int getStatusCount() {
		return statusCount;
	}

	public boolean getUniqueAmongRawResults() {
		return uniqueAmongRawResults;
	}

	public boolean getUniqueAmongFilteredResults() {
		return uniqueAmongFilteredResults;
	}
	
	public void setUniqueAmongFilteredResults(boolean unique) {
		uniqueAmongFilteredResults = unique;
	}
	
	public int getMatchScore() {
		return matchScore;
	}
}
