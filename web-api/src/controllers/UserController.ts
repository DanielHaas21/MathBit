import {
  Controller,
  Get,
  Post,
  Body,
  Route,
  Tags,
  SuccessResponse,
  Path,
  Security,
  Delete,
  Put,
} from 'tsoa';
import bcrypt from 'bcrypt';
import { type CreateUser } from '../dto/CreateUser';
import { type User } from '../dto/User';
import { UserRepository, User as DBuser } from 'db';
import { UserMapper } from '../mappers/UserMapper';
import { type UpdateUserRequest } from '../dto/Request/UserRequest';
/**
 * Controller for managing users.
 * Provides endpoints for creating and retrieving users.
 */
@Route('users')
@Tags('Users')
export class UserController extends Controller {
  private userRepository: UserRepository;

  constructor() {
    super();
    this.userRepository = new UserRepository();
  }
  /**
   * Creates a new user.
   * This endpoint accepts user data and creates a new user in the system.
   *
   * @param user The user data to create
   * @returns {User} The created user data
   */
  @SuccessResponse('201', 'Created')
  @Post()
  public async createUser(@Body() user: CreateUser): Promise<User | null> {
    // hash password before saving
    const hashedPassword = await bcrypt.hash(user.password, 10);
    const dbUser: DBuser = { ...user, password: hashedPassword };

    // to prevent duplicate emails
    const existingUser = await this.userRepository.getUserByEmail(user.email);
    if (existingUser) {
      this.setStatus(409); 
      return null;
    }

    const id = await this.userRepository.createUser(dbUser);
    if (id) {
      const newUser = await this.userRepository.getUserById(id);
      this.setStatus(201);
      if (!newUser) {
        throw new Error('User not found');
      }
      return UserMapper.toDto(newUser); // can never be null
    }

    throw new Error('User not created');
  }
  /**
   * Creates a new user.
   * This endpoint accepts user data and creates a new user in the system.
   *
   * @param user The user data to create
   * @returns {User} The created user data
   */
  @Get()
  @Security('jwt')
  public async getAllUsers(): Promise<{ users: User[] }> {
    const users = await this.userRepository.getAllUsers();
    return { users: users.map((u) => UserMapper.toDto(u)) as User[] };
  }
  /**
   * Retrieves a user by their ID.
   *
   * @param id The ID of the user to retrieve
   * @returns {User} The user data
   */
  @Get('{id}')
  @Security('jwt')
  public async getUserById(@Path() id: number): Promise<User | null> {
    const user = await this.userRepository.getUserById(id);
    if (!user) {
      throw new Error('User not found');
    }
    return UserMapper.toDto(user);
  }

  /**
   * Updates a User.
   * This endpoint accepts a body to update a user in the system
   *
   * @param body Update request; consists of id, and user data
   * @returns {void} no return value
   */
  @Put()
  @Security('jwt')
  public async updateUser(@Body() body: UpdateUserRequest): Promise<void> {
    await this.userRepository.updateUser(body.id, body.user);
  }

  /**
   * Deletes a User.
   * This endpoint accepts an id to delete a user in the system
   *
   * @param id id of the user
   * @returns {void} no return value
   */
  @Delete('{id}')
  @Security('jwt')
  public async deleteUser(@Path() id: number): Promise<void> {
    await this.userRepository.deleteUser(id);
  }
}
