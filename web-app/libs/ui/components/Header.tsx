import React from 'react';
import { logout as logoutAPI } from 'web-api-client';
import { Label } from './Label';
import { Icon } from './Icon';
import { Breadcrumb } from './Breadcrumb';
import { Dropdown, DropdownMenu } from './Dropdown';
import { AppDispatch, RootState } from '@/store/store';
import { useSelector, useDispatch } from 'react-redux';
import { Button } from './Button';
import { BreadcrumbItem } from '../types';
import { useTranslation } from '../provider';
import { logout } from '@/store/slices/UserState';
import { useNavigate } from 'react-router-dom';
import { Logo } from './Logo';
import getApiConfig from '@/apiConfig';
import { cn } from '../utils';
export interface HeaderProps {
  route: BreadcrumbItem[];
}

export const Header: React.FC<HeaderProps> = ({ route }) => {
  const Dispatch = useDispatch<AppDispatch>();
  const user = useSelector((state: RootState) => state.User);
  const navigate = useNavigate();
  const t = useTranslation('ui');

  const logoutHandler = async () => {
    try {
      await logoutAPI(getApiConfig(false));
    } finally {
      Dispatch(logout());
      navigate('/');
    }
  };

  const isLoggedIn = user.accessToken !== undefined ? true : false;
  const userSection = isLoggedIn ? (
    <DropdownMenu
      align="bottom"
      triggerButton={
        <div className="cursor-pointer rounded-full border-3 border-text-black flex items-center justify-center w-[60px] h-[60px]">
          <Icon name="user" className="text-[30px]"></Icon>
        </div>
      }
      HeadItem={
        <div className="flex flex-row gap-3 ">
          <div className="cursor-pointer rounded-full border-2 border-text-black flex items-center justify-center w-[35px] h-[35px]">
            <Icon name="user" className="text-[20px]"></Icon>
          </div>
          <div className="flex flex-col gap-0">
            <Label size="md" className="font-medium">
              {user.user?.username ?? 'username'}
            </Label>
            <Label size="xs" className="text-text-grey">
              {user.user?.email ?? 'example@example.com'}
            </Label>
          </div>
        </div>
      }
    >
      <Dropdown.Item
        icon="gear"
        onClick={() => navigate('/settings')}
        className="text-text-black m-2 hover:bg-brand-blue-50"
      >
        {t('header.settings')}
      </Dropdown.Item>
      <Dropdown.Item
        icon="user"
        onClick={logoutHandler}
        className="text-error-text m-2 hover:bg-error-bg"
      >
        {t('header.logout')}
      </Dropdown.Item>
    </DropdownMenu>
  ) : (
    <div className="flex w-full flex-row gap-2 mt-6">
      <Button variant="primary" className="w-full" onClick={() => navigate('/login')} size="md">
        {t('header.login')}
      </Button>
      <Button
        variant="primary"
        className="w-full"
        onClick={() => navigate('/signup')}
        outline={'primary'}
        size="md"
      >
        {t('header.signup')}
      </Button>
    </div>
  );

  return (
    <header className="w-full flex flex-col bg-white-50">
      <div
        className={cn(
          isLoggedIn
            ? 'w-full flex flex-row justify-between items-center px-5'
            : 'w-full flex flex-col sm:flex-row justify-between items-center px-5'
        )}
      >
        <Logo></Logo>
        <div
          className={cn(isLoggedIn ? 'pt-6 md:pr-[6vw] ' : 'pt-6 md:pr-[6vw] w-full sm:w-auto')}
        >
          {userSection}
        </div>
      </div>
      <Breadcrumb
        className="ps-10 pt-2 pb-2 md:pt-4 md:pb-0"
        route={[{ pageRoute: '/', pageTitle: <Icon name="house"></Icon> }, ...route]}
      ></Breadcrumb>
    </header>
  );
};
