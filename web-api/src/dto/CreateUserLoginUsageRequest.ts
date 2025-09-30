import { UserLoginUsage } from './UserLoginUsage';

/**
 * Represents the data usage of a user synchronization request
 */
export interface CreateUserLoginUsageRequest {
  /**
   * User usage item list
   */
  items?: UserLoginUsage[];
}
