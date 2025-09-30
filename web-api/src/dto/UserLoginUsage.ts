/**
 * Represents the data usage of a user
 */
export interface UserLoginUsage {
  /**
   * System username
   */
  userName: string;

  /**
   * Processed date
   */
  date?: Date | null;

  /**
   * User's last login
   */
  firstLogin?: Date | null;

  /**
   * User's last login
   */
  lastLogin?: Date | null;

  /**
   * Number of logins
   */
  loginCount: number | null;
}
