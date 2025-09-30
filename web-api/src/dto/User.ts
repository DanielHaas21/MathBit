/**
 * Represents a user in the system
 */
export interface User {
  /**
   * Unique identifier for the user
   */
  id?: number;
  /**
   * User's displayed username
   */
  username: string;
  /**
   * User's first name
   */
  firstName: string;

  /**
   * User's last name
   */
  lastName: string;

  /**
   * User's email address
   */
  email: string;

  /**
   * User' creation Date
   */
  created?: Date;
}
