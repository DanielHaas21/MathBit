import { MenuButton } from '../components';

export interface MenuWrapperItem {
  item: React.ReactElement<typeof MenuButton>;
  bordered?: boolean;
}
