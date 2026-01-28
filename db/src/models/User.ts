export interface User {
  id?: number;
  username: string;
  email: string;
  password: string;
  created?: Date; // autofilled
}
