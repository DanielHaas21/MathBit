import { Button, Header, Icon, SettingsForm, useToast } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import { useTranslation } from '@/libs/ui/provider';
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { RootState } from '@/store/store';
import { useSelector } from 'react-redux';
import { refresh } from 'web-api-client';
import getApiConfig from '@/apiConfig';
import update from '@/middleware/actions/update';
import { AnimatePresence, motion } from 'framer-motion';

export default function Settings() {
  const user = useSelector((state: RootState) => state.User);
  const t = useTranslation('pages.settings');
  const [serverError, setServerError] = useState<string | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
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
        <Header route={[{ locKey: 'settings', pageRoute: '/settings' }]} />
      </BaseLayout.Menu>
      <div>
        <Button
          onClick={() => navigate(-1)}
          outline={'primary'}
          size="md"
          className=" gap-2 ms-8 mt-8 mb-8"
        >
          <Icon name="arrow-right"></Icon> {t('goBack')}
        </Button>
      </div>
      <BaseLayout.Content className="justify-center items-center">
        <AnimatePresence>
          <motion.div
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -10 }}
            className="flex flex-col items-center"
            transition={{ duration: 0.6, ease: 'easeOut' }}
          >
            <SettingsForm
              data={{ username: user.user?.username!, email: user.user?.email! }}
              serverError={serverError}
              isSubmitting={loading}
              onSubmit={handleSignup}
            ></SettingsForm>
          </motion.div>
        </AnimatePresence>
      </BaseLayout.Content>
    </BaseLayout>
  );
}
