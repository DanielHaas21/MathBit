import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import {
  createUser,
  deleteUser,
  getAllUsers,
  getUserById,
  login,
  LoginResponse,
  updateUser,
  type CreateUser,
  type UpdateUserRequest,
} from 'web-api-client';
import { getApiConfig } from 'src/utils/getApiConfig';

const testEmail = `testuser_${Date.now()}@example.com`; // unique email to avoid conflicts if tests are re-run without cleanup or fail midway
const testPassword = 'Test123!'; // "strong" password for testing
let createdUserId: number;
let accessToken: string;
describe('UserController Integration Roundtrip', () => {
  it('should create a new user', async () => {
    const newUser: CreateUser = { email: testEmail, password: testPassword, username: 'Test User' };
    const created = await createUser(newUser, getApiConfig(true, accessToken));

    expect(created).toBeDefined();
    expect(created?.email).toBe(testEmail);

    createdUserId = created?.id as number;
  });

  it('should login with created user', async () => {
    const response: LoginResponse = await login(
      { email: testEmail, password: testPassword },
      getApiConfig(false, accessToken)
    );

    expect(response.accessToken).toBeDefined();
    expect(response.userProfile?.email).toBe(testEmail);

    accessToken = response.accessToken!;
  });

  it('should fetch the created user by ID', async () => {
    const fetched = await getUserById(createdUserId, getApiConfig(false, accessToken));

    expect(fetched).toBeDefined();
    expect(fetched?.email).toBe(testEmail);
  });

  it('should fetch all users', async () => {
    const allUsers = await getAllUsers(getApiConfig(false, accessToken));
    expect(allUsers.users).toEqual(
      expect.arrayContaining([expect.objectContaining({ email: testEmail })])
    );
  });

  it('should update the user', async () => {
    const updateBody: UpdateUserRequest = {
      id: createdUserId,
      user: { username: 'Updated Name', email: testEmail, password: testPassword },
    };
    await updateUser(updateBody, getApiConfig(false, accessToken));
    const updated = await getUserById(createdUserId, getApiConfig(false, accessToken));
    expect(updated?.username).toBe('Updated Name');
  });

  it('should delete the user', async () => {
    await deleteUser(createdUserId, getApiConfig(false, accessToken));

    // Optionally, verify deletion
    await expect(getUserById(createdUserId, getApiConfig(false, accessToken))).rejects.toThrow();
  });
});
