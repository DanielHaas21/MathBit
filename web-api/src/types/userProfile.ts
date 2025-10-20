/**
 * Represents a user's profile information.
 */
export interface UserProfile {
  /**
   * User's displayed username
   */
  username?: string | null | undefined;
  /**
   * User's email address
   */
  email?: string | null | undefined;
  /**
   * Unique identifier for the user
   */
  id?: number | null;
  /**
   * User's first name
   */
  firstName?: string | null | undefined;
  /**
   * User's last name
   */
  lastName?: string | null | undefined;
}
