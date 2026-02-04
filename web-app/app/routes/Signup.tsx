import { Button, Icon, Logo } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import { useTranslation } from '@/libs/ui/provider';
import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import signup from '@/middleware/auth/signup';
import { SignupForm } from '@/libs/ui/components/SignupForn';
import { AnimatePresence, motion } from 'framer-motion';

export default function Signup() {
  const t = useTranslation('pages.signup');
  const [serverError, setServerError] = useState<string | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const navigate = useNavigate();

  const handleSignup = async (username: string, email: string, password: string) => {
    setLoading(true);
    setServerError(null);
    const result = await signup({ username, email, password });

    setLoading(false);

    if (result.ok === true) {
      navigate('/browser');
      return;
    }

    if (result?.errorCode) {
      setServerError(t(`errors.${result.errorCode}`));
    } else {
      setServerError(t('errors.generic'));
    }
  };
  return (
    <BaseLayout className="justify-center items-center">
      <Button
        onClick={() => navigate('/')}
        outline={'primary'}
        size="md"
        className="absolute top-8 gap-2 left-8"
      >
        <Icon name="arrow-right"></Icon> {t('goBack')}
      </Button>
      <AnimatePresence>
        <motion.div
          initial={{ opacity: 0, y: -10 }}
          animate={{ opacity: 1, y: 0 }}
          exit={{ opacity: 0, y: -10 }}
          className="flex flex-col items-center"
          transition={{ duration: 0.6, ease: 'easeOut' }}
        >
          <Logo className="pb-4"></Logo>
          <SignupForm
            serverError={serverError}
            isSubmitting={loading}
            onSubmit={handleSignup}
          ></SignupForm>
        </motion.div>
      </AnimatePresence>
    </BaseLayout>
  );
}
