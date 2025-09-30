export interface UserLogin {
  id?: number;
  userName?: string;
  firstLogin?: Date | null;
  lastLogin?: Date | null;
  loginCount: number | null;
}
