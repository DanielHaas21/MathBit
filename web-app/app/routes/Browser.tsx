import { Header } from '@/libs/ui/components/Header';
import { BaseLayout } from '@/libs/ui/layouts';
import { getBrowser } from '../navigation';

export default function Browser() {
  return (
    <BaseLayout>
      <BaseLayout.Menu>
        <Header route={[{ pageTitle: 'Browser', pageRoute: getBrowser() }]} />
      </BaseLayout.Menu>
      <BaseLayout.Content>s</BaseLayout.Content>
    </BaseLayout>
  );
}
