import {
  Header,
  Label,
  LoginForm,
  Logo,
  MathField,
  SettingsForm,
  useToast,
} from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import logo from '../../libs/ui/assets/images/dark/logo_medium.svg';
import { useTranslation } from '@/libs/ui/provider';
import { use, useEffect, useState } from 'react';
import login from '@/middleware/auth/login';
import { useNavigate } from 'react-router-dom';
import signup from '@/middleware/auth/signup';
import { SignupForm } from '@/libs/ui/components/SignupForn';
import { RootState } from '@/store/store';
import { useSelector } from 'react-redux';
import { refresh } from 'web-api-client';
import getApiConfig from '@/apiConfig';
import update from '@/middleware/actions/update';

export default function Settings() {
  const user = useSelector((state: RootState) => state.User);
  const t = useTranslation('pages.settings');
  const [serverError, setServerError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const navigate = useNavigate();
  const { show } = useToast();
  useEffect(() => {
    const tryRefresh = async () => {
      const response = await refresh(getApiConfig(true));
      if (response.errorCode) navigate('/login');
    };
    tryRefresh();
  }, [user]);

  const handleSignup = async (username: string, email: string, password: string) => {
    setLoading(true);
    setServerError(null);
    if (!user.user?.id) return;

    const result = await update(user.user?.id, {
      username: username,
      email: email,
      password: password,
    });

    setLoading(false);

    if (result.ok === true) {
      show({ variant: 'success', title: t('messages.updateSuccess') });
      return;
    }

    if (result?.errorCode) {
      setServerError(t(`errors.${result.errorCode}`));
    } else {
      setServerError(t('errors.generic'));
    }
  };
  return (
    <BaseLayout>
      <BaseLayout.Menu>
        <Header route={[{ pageTitle: 'Settings', pageRoute: '/settings' }]} />
      </BaseLayout.Menu>
      <BaseLayout.Content className="justify-center items-center">
        <SettingsForm
          data={{ username: user.user?.username!, email: user.user?.email! }}
          serverError={serverError}
          isSubmitting={loading}
          onSubmit={handleSignup}
        ></SettingsForm>
      </BaseLayout.Content>
    </BaseLayout>
  );
}
